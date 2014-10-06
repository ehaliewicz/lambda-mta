;; closure.lisp
;; convert each function into a closure that keeps track of it's free variables

(in-package #:lambda-mta)


(defun closure-convert-ast (cps-ast)
  (closure-convert cps-ast (list) (list)))

(defgeneric closure-convert (ast env free-vars)
  (:documentation
   "Performs closure conversion of ast expression.
    Returns (values converted-expr, free-vars-in-expr)."))


(defmethod closure-convert ((ast sym) env free-vars)
  (if (find ast env :test 'eql)
      (values ast free-vars)
      (values ast (union (list ast) free-vars :test 'eql))))


(defmethod closure-convert ((ast func) env free-vars)
  (with-slots (args body str) ast
    (multiple-value-bind (cc-body body-freevars)
        ;; set environment to this function's arguments
        (closure-convert body args free-vars)
      ;; get all free-vars
      (let ((cur-free-vars (union free-vars body-freevars
                                  :test 'eql)))
        ;; remove any of this function's arguments
        ;; that were added in above recursions
        (loop for arg in args do
             (setf cur-free-vars (remove arg cur-free-vars)))

        (values (make-closure :freevars cur-free-vars
                              :args args
                              :body cc-body
                              :str str)
                cur-free-vars)))))


;; convert applications with 1 operand
;; (at this point, that's only applications of continuations to function return values
;; applications in the original source have two operands
;; i.e. (function operand continuation)
(defmethod closure-convert ((ast app) env free-vars)
  (with-slots (operator operand) ast
    (multiple-value-bind
          (cc-operator operator-fvs) (closure-convert operator env free-vars)
      (multiple-value-bind
            (cc-operand operand-fvs) (closure-convert operand env free-vars)
        (values (make-app :operator cc-operator
                          :operand cc-operand)
                (union operator-fvs
                       operand-fvs
                       :test 'eql))))))

;; convert applications of the form (function operand continuation)
(defmethod closure-convert ((ast multi-app) env free-vars)
  (with-slots (operator operands) ast
    (assert (= 2 (length operands)))
    (destructuring-bind (op-a op-b) operands
      (multiple-value-bind
            (cc-operator operator-fvs) (closure-convert operator env free-vars)
        (multiple-value-bind (cc-a a-fvs) (closure-convert op-a env free-vars)
          (multiple-value-bind (cc-b b-fvs) (closure-convert op-b env free-vars)
            (values (make-multi-app :operator cc-operator
                                    :operands (list cc-a cc-b))
                    (union operator-fvs
                           (union a-fvs b-fvs :test 'eql)
                           :test 'eql))))))))

