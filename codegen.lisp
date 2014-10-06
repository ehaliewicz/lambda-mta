;; generates C code to execute the given AST expression

;; sets up a soft stack limit
;; creates a trampoline via setjmp
;; and allocates all closures on the C stack

;; outputs C in CPS form, with no function returns,
;; so stack allocated objects survive function boundaries

;; when the soft stack limit is hit,
;; all accessible objects (just arguments for the current function)
;; are copied to the heap,
;; we save a pointer to the current function (along with copied arguments),
;; return to the trampoline via longjmp
;; wiping out the C stack and all dead objects
;; and resume the paused function

(in-package #:lambda-mta)


(defun compile-to-file (expr file &optional (stack-size "0x800000"))
  (with-open-file (fout file :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format fout "~a" (code-gen (closure-convert-ast (ast->cps (expr->ast expr)))
                                stack-size))))

(defun compile-to-binary (expr file &optional (stack-size "0x800000"))
  (compile-to-file expr (format nil "~a.c" file) stack-size)
  (sleep 2)
  (trivial-shell:shell-command (format nil "cc ~a.c -o ~a" file file)))

(defun compile-and-execute (expr file &optional (stack-size "0x800000"))
  (compile-to-binary expr file stack-size)
  (trivial-shell:shell-command file))




;; generate includes, typedefs, utility functions etc.
(defun util-defs (stack-size)
  (append (generate-includes)
          (generate-macros stack-size)
          (generate-structs)
          (generate-static-vars)
          (generate-utility-functions)))

(defun generate-includes ()
  '("#include \"stdio.h\""
    "#include \"stdlib.h\""
    "#include \"string.h\""
    "#include \"setjmp.h\""
    "#include \"sys/resource.h\" /* for hard stack limit */"
    ""))

(defun generate-macros (stack-size)
  (list
   (format nil "#define STACK_SIZE ~a" stack-size)
   "#define MULTI_LINE_STRING(a) #a"
   ""))

(defun generate-structs ()
  '("typedef struct {"
    "  void* apply_func;" ;; pointer function to apply the correct number of arguments
    "  void* func;"  ;; pointer to function implementing this closure
    "  void* forward;" ;; forward pointer (for GC)
    "  int tenured;" ;; tenured flag (for GC)
    "  char* str;" ;; string representation
    "  int num_freevars;" ;; number of free-variables captured
    "  void* bound_args[];" ;; array of bound free-variables (clos*)
    "} clos;"
    ""
    ;; represents a function to re-try
    ;; after returning to the trampoline function
    "typedef struct {"
    "  void* func;"   ;; saved function pointer
    "  int num_args;" ;; number of args to call with
    "  clos* args[];" ;; saved arguments
    "} call;"
    ""
    ;; enum for which pool we're currently using
    "typedef enum { POOL_A, POOL_B } cur_pool_enum;"
    ""))

(defun generate-static-vars ()
  (list
   ;; flag indicating current memory pool
   ;; (2 needed for major GC, which is not yet implemented)
   "static cur_pool_enum cur_pool;"
   "static char* mem_pool_a;"
   "static char* mem_pool_b;"
   ;; pointers to ends of memory pools
   "static char* mem_pool_a_end;"
   "static char* mem_pool_b_end;"
   ;; allocation pointer to current memory pool
   "static char* cur_mem_ptr;"
   ;; pointer to end of current memory pool
   "static char* cur_mem_pool_end;"
   ;; current stack pointer
   "static char* cur_stack;"
   ;; end of soft stack limit
   "static char* stack_end;"
   ;; stack direction flag
   "static int stack_grows_up;"
   ""))



(defun generate-utility-functions ()
  '("void cleanup() {"
    "  free(mem_pool_a);"
    "  free(mem_pool_b);"
    "}"
    "void halt_func(clos* halt, clos* val) {"
    "  printf(\"%s\\n\", val->str);"
    "  cleanup();"
    "  exit(0);"
    "}"
    ;; closure for halting program
    "clos halt_impl = {&halt_func, /* apply func */"
    "                  NULL, /* func */"
    "                  NULL, /* forward pointer */"
    "                     0, /* tenured */"
    "                  \"halt func\", /* func string */"
    "                     0, /* num_freevars */"
    "                  NULL}; // no bound args"
    "static clos* halt = &halt_impl;"
    ""
    "/* major GC not yet implemented */"
    "void switch_pools() {"
    "  printf(\"switching pools\\n\");"
    "  if(cur_pool == POOL_A) { "
    "    cur_mem_ptr = mem_pool_b;"
    "    cur_mem_pool_end = mem_pool_b_end;"
    "  } else { "
    "    cur_mem_ptr = mem_pool_a;"
    "    cur_mem_pool_end = mem_pool_a_end;"
    "  }"
    "}"
    ;; check current stack pointer against stack limit
    ;; returns 1 if exceeding stack limit
    "int check_stack() {"
    "  if(stack_grows_up) {"
    "    return cur_stack >= stack_end;"
    "  } else { "
    "    return cur_stack <= stack_end;"
    "  }"
    "}"
    

    ;; forward declaration of dynamically generated functions
    ;; perform call resumes calls saved directly before GC
    "void perform_call(call* call);"
    ;; toplevel of generated code for lambda expression
    "void toplevel();"

    ;; dynamically allocated space to pause calls
    "static call* saved_cont_call;"
    ;; memory to save stack pointer + register state
    "jmp_buf empty_stack_state;"

    ;; variable to test if stack is big enough
    ;; to allocate all closures in a function
    "static int mem_needed;"

    
    ;; sets up trampoline via setjmp,
    ;; sets up stack position and calls toplevel via perform_call
    
    ;; if we return to trampoline here via longjmp
    ;; load saved call
    ;; reset stack position
    ;; and call saved function via perform_call
    "void trampoline(call* cl) {"
    " /* save stack pointer and registers (including program counter) */"
    "  if(setjmp(empty_stack_state)) {"
    "    /* if got here by longjump, load saved call (to continue after GC) */"
    "    cl = saved_cont_call; "
    "    if(STACK_SIZE < mem_needed) { "
    "      printf(\"Stack size too small!\\n\");"
    "      cleanup();"
    "      exit(0);"
    "    }"
    "  }"
    ""
    ;; set up stack (infant generation) parameters
    "  cur_stack = ((char*)alloca(1));"
    "  char* last_stack = cur_stack;"
    "  cur_stack = ((char*)alloca(1));"
    "  stack_grows_up = (cur_stack > last_stack);"
    "  if (stack_grows_up) {"
    "    stack_end = last_stack + STACK_SIZE;"
    "  } else {"
    "    stack_end = last_stack - STACK_SIZE;"
    "  }"
    "  perform_call(cl);"
    "}"

    "void entry_point() {"
    ;; allocate toplevel_call
    "  call* toplevel_call = alloca(sizeof(call));"
    "  toplevel_call->func = &toplevel;"
    "  toplevel_call->num_args = 0;"
    ;; jump to trampoline
    "  trampoline(toplevel_call);"
    "}"
    ""


    ;; allocates heap memory, bumps allocation pointer
    ;; exits program when memory is exhausted
    "char* pool_alloc(int num_bytes) {"
    "  char* new_ptr = (cur_mem_ptr+num_bytes);"
    "  if (new_ptr >= cur_mem_pool_end) {"
    "    printf(\"Out of memory!\\n\");"
    "    cleanup();"
    "    exit(0);"
    "  } else { "
    "    char* ret_ptr = cur_mem_ptr;"
    "    cur_mem_ptr = new_ptr;"
    "    return ret_ptr;"
    "  }"
    "}"

    ;; heart of the gc algorithm
    ;; recursively copies objects to the heap

    ;; if the object's forward pointer is set, return that
    
    ;; else check if the object is already in the heap, if so, returns
    ;; it's address (this only works for minor gc)

    ;; otherwise, copy the object to the heap
    ;; set the original object's forward pointer to the heap address
    ;; recursively copy it's fields (bound arguments in the case of a closure)
    ;; and finally return it's new address in the heap
    "static unsigned long bytes_copied;"
    ""
    "clos* copy(clos* cls) {"
    " /* if forward pointer is unset, copy closure,"
    "    set forward address to copied closure's address"
    "    return return forward address  */"
    "  if(cls->forward == NULL) {"
    "    /* if tenured, just return pointer */"
    "    /* it's safe from the incoming stack crunch */"
    "    if(cls->tenured == 1) { return cls; }"
    "    int num_freevars = cls->num_freevars;"
    "    int freevars_size = (sizeof(clos*)*num_freevars);"
    "    int clos_size = sizeof(clos)+freevars_size+sizeof(char*);"
    ""
    "    bytes_copied += clos_size;"
    ""
    "    /* allocate memory for closure in tenured generation */"
    "    clos* new_cls = (clos*)(pool_alloc(clos_size));"
    ""
    "    /* set forward pointer for further copy calls that reach this closure */"
    "    cls->forward = new_cls;"
    "    new_cls->tenured = 1;"
    "    /* copy closure fields */"
    "    new_cls->apply_func = cls->apply_func;"
    "    new_cls->func = cls->func;"
    "    new_cls->num_freevars = cls->num_freevars;"
    "    /* copy string pointer (i think we can assume it's allocated statically) */"
    "    new_cls->str = cls->str;"
    "    new_cls->forward = NULL;"
    "    /* copy bound arguments */"
    "    for(int i = 0; i < num_freevars; i++) {"
    "      new_cls->bound_args[i] = copy(cls->bound_args[i]);"
    "    }"
    "  }"
    "  return cls->forward;"
    "}"    

    
    ;; recursively copy all arguments in the currently saved call
    "void minor_gc() {"
    "  bytes_copied = 0;"
    "  int num_args = saved_cont_call->num_args;"
    "  for(int i = 0; i < num_args; i++) {"
    "    saved_cont_call->args[i] = copy(saved_cont_call->args[i]);"
    "  }"
    "  printf(\"bytes_copied: %lu\\n\", bytes_copied);"
    ;; jump back to trampoline
    "  longjmp(empty_stack_state, 1);"
    "}"
    ""))


;; for pretty printing
(defvar *indent*)

(defun emit (strm str &rest args)
  ;; add *indent* spaces
  (dotimes (i *indent*)
    (format strm " "))
  (apply #'format strm str args))


(defun code-gen (ast stack-size)
  (let ((*indent* 0)
        (*forward-declarations* (list))
        (*perform-calls-needed* (list 0))
        (*typedef-table* (make-hash-table :test 'equal))
        (*typedefs* (list))
        (*function-definitions* (list))
        (*apply-func-table* (make-hash-table :test 'equal))
        (*apply-func-defs* (list)))
    (declare (special *indent* *forward-declarations*
                      *perform-calls-needed* *typedef-table*
                      *typedefs* *function-definitions*
                      *apply-func-table* *apply-func-defs*))
    ;; typedef for toplevel
    (generate-fp-typedef 0)
    ;; generate all code recursively
    (generate-function-definition "toplevel" (make-closure
                                              :freevars (list)
                                              :args (list)
                                              :body ast
                                              :str "toplevel function"))

    (with-output-to-string (out)
      ;; emit utility definitions
      (dolist (decl (util-defs stack-size))
        (emit out "~a~%" decl))

      ;; emit generated typedefs
      (emit out "/* typedefs */~%")
      (dolist (decl *typedefs*)
        (emit out "~a~%" decl))

      ;; emit apply functions
      (emit out "~%/* apply function definitions */~%")
      (dolist (decl *apply-func-defs*)
        (emit out "~a~%" decl))

      ;; emit lambda function forward declarations
      (emit out "~%/* forward declarations */~%")
      (dolist (decl *forward-declarations*)
        (emit out "~a~%" decl))

      ;; emit lifted function definitions
      (emit out "~%/* lifted function definitions */~%")
      (dolist (decl *function-definitions*)
        (emit out "~a~%" decl))
      
      ;; generate apply_call function
      ;; with switch/case jump table
      (emit out "void perform_call(call* cl) {~%")
      (emit out "  switch(cl->num_args) {~%")
      (dolist (num-args (sort *perform-calls-needed* #'<))
        (emit out "  case ~a:~%" num-args)
        (emit out "    ((~a)cl->func)(~{~a~^,~});~%"
              (gethash num-args *typedef-table*)
              (loop for i below num-args collect
                   (emit nil "cl->args[~a]" i)))
        (emit out "    break;~%"))
      (emit out "  }~%")
      (emit out "}~%")

      
      (emit out "~%int main() { ~%")
      (let ((*indent* 2))

        (emit out "struct rlimit rl;~%")
        (emit out "rl.rlim_cur = STACK_SIZE*2;~%")
        (emit out "setrlimit(RLIMIT_STACK, &rl);~%")
            
        ;; allocate space for tenured generation
        (emit out "mem_pool_a = malloc(1024*1024*8);~%")
        (emit out "mem_pool_b = malloc(1024*1024*1024);~%")
        (emit out "mem_pool_a_end = (mem_pool_a+(1024*1024*8));~%")
        (emit out "mem_pool_b_end = (mem_pool_b+(1024*1024*8));~%")

        (emit out "cur_mem_ptr = mem_pool_a;~%")
        (emit out "cur_mem_pool_end = mem_pool_a_end;~%")
        (emit out "saved_cont_call = malloc(sizeof(call)+(sizeof(void*)*~a));~%"
              (reduce #'max *perform-calls-needed*))
            

        (emit out "entry_point();~%")
            
        (emit out "return 0;~%"))
      (emit out "}"))))


;; returns a list of lines for declarations
;; so that we can indent each line correctly 
(defgeneric generate-code (expr)
  (:documentation "Generates C code for the given ast expression.
Returns (values lines-of-declaration-text, value-text, memory-allocated-for-expr-text)"))


;; generate code for a symbol (variable reference)
(defmethod generate-code ((expr sym))
  (values nil ;; no declaration to reference a variable
          (string (sym-name expr))
          nil)) ;; no memory cost

(defmethod generate-code ((expr app))
  (with-slots (operator operand) expr
    (multiple-value-bind (operator-decl-lines
                          operator-value
                          operator-mem) (generate-code operator)
      (multiple-value-bind (operand-decl-lines
                            operand-value
                            operand-mem) (generate-code operand)
        (values
         ;; DECLARATION: whatever needed for operator and operand
         (append operand-decl-lines operator-decl-lines)
         ;; VALUE: apply operator to operand via the closure's apply_func
         (emit nil "((~a)(~a->apply_func))(~a,~a)"
               (generate-fp-typedef 2) ;; make sure the typedef is generated
               operator-value operator-value operand-value)
         (append operator-mem operand-mem))))))

(defmethod generate-code ((expr multi-app))
  (with-slots (operator operands) expr
    (assert (= 2 (length operands)))
    (destructuring-bind (op-a op-b) operands
      (multiple-value-bind (operator-decl-lines
                            operator-value
                            operator-mem)
          (generate-code operator)
        (multiple-value-bind (operand-a-decl-lines
                              operand-a-value
                              operand-a-mem)
            (generate-code op-a)
          (multiple-value-bind (operand-b-decl-lines
                                operand-b-value
                                operand-b-mem)
              (generate-code op-b)
            (values
             ;; DECLARATION: operator + operand declarations
             (append operand-a-decl-lines
                     operand-b-decl-lines
                     operator-decl-lines)
                                                          
             ;; VALUE: apply function to operand and continuation
             ;; via the closure's apply_func
             (emit nil "((~a)(~a->apply_func))(~a,~a,~a)"
                   (generate-fp-typedef 3)
                   operator-value operator-value operand-a-value operand-b-value)
             ;; MEMORY_NEEDED: memory needed for operator + operand declarations
             (append operator-mem
                     operand-a-mem
                     operand-b-mem))))))))

;; generate code for closures
(defmethod generate-code ((expr closure))
  ;; create function definition
  ;; and traverse body of function
  (with-slots (freevars args body str) expr
    (let* ((num-args (+ (length freevars) (length args)))
           (func-sym (gensym "func"))
           (clos-sym (gensym "clos")))
      (generate-function-forward-decl func-sym num-args)
      (generate-function-definition func-sym expr)
      
      (let ((size-sym (string (gensym "size"))))
        (values
         ;; DECLARATION: declare closure, set fields, etc.
         (append
          (list
           ;; calculated needed size
           (emit nil "int ~a = (sizeof(clos)+(sizeof(clos*)*~a)+(sizeof(char*)));"
                 size-sym (length freevars)(length str))
           ;; allocate memory on stack
           (emit nil "clos* ~a = alloca(~a);" clos-sym size-sym)
           ;; set number of free variables
           (emit nil "~a->num_freevars = ~a;" clos-sym (length freevars))
           ;; set func pointer
           (emit nil "~a->func = &~a;" clos-sym func-sym)
           ;; set pointer to function to apply this closure to arguments + freevars
           (let ((apply-func-sym (generate-apply-func args freevars)))
             (emit nil "~a->apply_func = &~a;" clos-sym apply-func-sym))
           ;; zero initialize forward pointer and tenured flag
           (emit nil "~a->forward = NULL;" clos-sym)
           (emit nil "~a->tenured = 0;" clos-sym)

           ;; set string representation field
           (if str
               ;; multi-line-string macro
               (emit nil "~a->str = MULTI_LINE_STRING(~a);"
                     clos-sym str)
               (emit nil "~a->str = NULL;"
                     clos-sym str)))
          ;; set up bound free variables
          (loop for i below (length freevars) collect
               (emit nil "~a->bound_args[~a] = (void*)~a;"
                     clos-sym i (sym-name (elt freevars i)))))
         
         ;; VALUE: closure variable name
         (emit nil "~a" clos-sym)
         ;; MEMORY-NEEDED: size of allocated closure
         (list (emit nil "(sizeof(clos)+(sizeof(clos*)*~a)+(sizeof(char)*~a))"
                     (length freevars)
                     (length str))))))))


;; list of forward declarations of generated lambda functions
(defvar *forward-declarations*)
;; list of numbers of arguments we need to be able to handle in perform_call
(defvar *perform-calls-needed*)
(defun generate-function-forward-decl (func-sym num-args)
  (setf *perform-calls-needed*
        (union (list num-args)
               *perform-calls-needed*))
    ;; generate the function pointer typedef for this function
  (generate-fp-typedef num-args)
  (push (emit nil "void ~a(~{~a~^, ~});" func-sym
              (loop for i below num-args collect "clos*"))
        *forward-declarations*))

;; maps number of args+freevars to function pointer typedef declarations
(defvar *typedef-table*)
;; list of function pointer typedefs
(defvar *typedefs*) 
(defun generate-fp-typedef (num)
  (if (gethash num *typedef-table*)
      (gethash num *typedef-table*)
      (let* ((sym (gensym "fp_typedef"))
             (decl (emit nil "typedef void (*~a)(~{~a~^, ~});"
                         sym (loop for i below num collect "clos*"))))
        (push decl *typedefs*)
        (setf (gethash num *typedef-table*) sym))))

;; list of function definitions
(defvar *function-definitions*)
;; only function where we use the third return value of #'generate-code
(defun generate-function-definition (func-name closure)
  (let ((*indent* 0))
    (declare (special *indent*))
    (let* ((definition
            (with-output-to-string (out)
              (with-slots (freevars args body str) closure
                (emit out "void ~a(~{clos* ~a~^, ~}) {~%" func-name
                      (mapcar #'sym-name (append args freevars)))
                (let ((*indent* 2)
                      (current-definition-arglist (append args freevars)))
                  (declare (special *indent*))
                  (multiple-value-bind (decl-lines val mem-list) (generate-code body)

                    (when mem-list
                      (emit out "/* check if allocation for this function fits on the stack */~%")
                      ;; make sure we have more than
                      ;; enough memory (2x just to be sure)
                      ;; because of the stack frame and such
                      (emit out "mem_needed = (~{~a~^+~})*2;~%" mem-list)
                      ;; bump stack pointer
                      (emit out "if (stack_grows_up) {~%")
                      (emit out "  cur_stack+= mem_needed;~%")
                      (emit out "} else { ~%")
                      (emit out "  cur_stack-= mem_needed;~%" mem-list)
                      (emit out "}~%")

                      ;; check if stack limit hit
                      (emit out "if(check_stack()) {~%")
                      (emit out "  /* GC + save call */~%")
                      (emit out "  printf(\"Soft stack size limit reached, GCing\\n\");~%")
                      ;; if it has, save current function's arguments
                      (loop for arg in current-definition-arglist
                         for i below (length current-definition-arglist) collect
                           (let ((name (string (sym-name arg))))
                             (emit out "  saved_cont_call->args[~a] = ~a;~%" i name)))

                      ;; save number of args
                      (emit out "  saved_cont_call->num_args = ~a;~%"
                            (length current-definition-arglist))
                      ;; save pointer to current function
                      (emit out "  saved_cont_call->func = ~a;~%" func-name)
                      ;; perform garbage collection
                      (emit out "  minor_gc();")
                      (emit out "}~%"))
                    
                    ;; emit declaration lines for body of function
                    (when decl-lines
                      (dolist (decl decl-lines)
                        (emit out "~a~%" decl)))
                    ;; emit value/expression for body of function
                    (emit out "~a;~%" val)))
                (emit out "}~%")))))
      (push definition *function-definitions*))))

;; maps (#-args, #-freevars) to symbol of apply function
(defvar *apply-func-table*)
(defvar *apply-func-defs*)
(defun generate-apply-func (args free-vars)
  (let* ((num-args (+ (length args) (length free-vars)))
         ;; typedef for function to call inside the
         ;; generated apply function
         (typedef (generate-fp-typedef (+ (length free-vars) (length args)))))
    (let ((sym (gethash (cons (length args) (length free-vars))
                        *apply-func-table*))
          (*indent* 0))
      (declare (special *indent*))
      (if sym
          sym
          (let ((arg-names (mapcar (lambda (a) (declare (ignore a)) (string (gensym "arg")))
                                   args))
                (func-sym (gensym (emit nil "apply_func~a" num-args))))

            (push
             (with-output-to-string (out)
               ;; no continuation handling required?
               (emit out "void ~a(clos* cls, ~{clos* ~a~^,~}) {~%"
                     func-sym
                     arg-names)
               ;; clos->func(arg, .. free-vars)
               (emit out "  return ((~a)(cls->func))(~{~a~^,~});~%"
                     typedef
                     (append arg-names
                             (loop for i below (length free-vars) collect
                                  (emit nil "((clos*)(cls->bound_args[~a]))" i))))
               (emit out "}~%"))
             *apply-func-defs*)
                         
            (setf (gethash (cons (length args)
                                 (length free-vars)) *apply-func-table*)
                  func-sym)
            func-sym)))))
