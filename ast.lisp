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


(defun expr->ast (expr)
  (astify expr (list)))


(defgeneric astify (expr env)
  (:documentation "Parses the given sexpr into an AST."))

(defmethod astify ((expr symbol) (env list))
  (if (assoc expr env :test 'eql)
      (cdr (assoc expr env :test 'eql))
      (error "Unbound symbol at compile-time: ~a" expr)))


(defmethod astify ((expr list) (env list))
  (destructuring-bind (operator &rest operands) expr
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
        :operand (recur (car operands) env))))))
