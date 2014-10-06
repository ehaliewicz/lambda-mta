;; ast.lisp
;; parse sexpr into a struct-based ast representation
(in-package #:lambda-mta)

;; symbol
;; (this struct is just so we can differentiate between eql symbols) 
(defstruct sym
  name)

;; one-operand application
(defstruct app
  operator
  operand)

;; two-operand application
;; (exists to simplify code generation of single-operand applications)
(defstruct multi-app
  operator
  operands)

;; lambda function (more than one arg possible due to CPS transform)
(defstruct func
  args
  body
  str)  ;; string representation of this function

;; function with a list of bound free-variables
(defstruct closure
  freevars
  args
  body
  str) ;; string representation


;; recursively parse into ast structs
(defun expr->ast (expr)
  (labels ((recur (expr env) ;; sexpr, (list of symbols)
             (etypecase expr
               (symbol (if (assoc expr env :test 'eql)
                           (cdr (assoc expr env :test 'eql))
                           (error "Unbound symbol at compile-time: ~a" expr)))
               
               (list (destructuring-bind (operator &rest operands) expr
                       (case operator
                         ;; lambda expression
                         (lambda (destructuring-bind (lambda (arg) body) expr
                              (declare (ignore lambda))
                              (let ((arg-sym (make-sym :name arg)))
                                (make-func
                                 :args (list arg-sym)
                                 :body (recur body
                                              (cons (cons arg arg-sym) env))
                                 :str expr))))
                         ;; application
                         (otherwise
                          (make-app
                           :operator (recur operator env)
                           :operand (recur (car operands) env)))))))))
    (recur expr (list))))
