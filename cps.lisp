;; cps.lisp
;; performs CPS transform of ast

;; uses the hybrid transform discussed at http://matt.might.net/articles/cps-conversion/

(in-package #:lambda-mta)

(defun ast->cps (ast)
  (cps-c ast (make-sym :name 'halt)))

(defgeneric cps-c (ast cont)
  (:documentation "Performs standard CPS transform on ast."))

;; (cps-c '(lambda (a) a) k) =>  '(k (lambda (a) a)) 
(defmethod cps-c ((ast func) (cont sym))
  ;; make application of cont
  ;; to recursively transformed ast
  (make-app :operator cont
            :operand (atomic->cps ast)))

;; (cps-c 'a 'k) => '(k a)
(defmethod cps-c ((ast sym) (cont sym))
  ;; apply cont to symbol
  (make-app :operator cont
            :operand (atomic->cps ast)))


(defmethod cps-c ((ast app) (cont sym))
  (with-slots (operator operand) ast
    (cps-k operator
           (lambda ($f)
             (cps-k operand 
                    (lambda ($e)
                      (make-multi-app
                       :operator $f
                       :operands (list $e cont))))))))



(defgeneric cps-k (ast cont)
  (:documentation "Performs higher-order CPS transform on ast."))

(defmethod cps-k ((ast func) (cont function))
  (funcall cont (atomic->cps ast)))

(defmethod cps-k ((ast sym) (cont function))
  (funcall cont (atomic->cps ast)))

(defmethod cps-k ((ast app) (cont function))
  (with-slots (operator operand) ast
    (let* ((sym (gensym "rv"))
           ($rv (make-sym :name sym))
           (cont (make-func
                  :args (list $rv)
                  :body (funcall cont $rv)
                  :str nil)))
      (cps-k operator (lambda ($f)
                        (cps-k operand (lambda ($e)
                                         ;; apply operator to operand
                                         ;; and continuation
                                         (make-multi-app
                                          :operator $f
                                          :operands (list $e cont)))))))))


(defgeneric atomic->cps (ast)
  (:documentation "Converts an atomic value (function or symbol) to an atomic CPS value."))

(defmethod atomic->cps ((ast func))
  ;; add continuation parameter to function
  (with-slots (args body str) ast
    (let* ((sym (gensym "k"))
           ($k (make-sym :name sym)))
      (assert (= 1 (length args)))

      (make-func :args (list (car args) $k)
                 ;; and recursively transform function
                 :body (cps-c body $k)
                 ;; keep original (non-cps) string representation
                 ;; functions generated in the CPS transformation do not
                 ;; keep a string representation
                 :str (prin1-to-string str)))))

(defmethod atomic->cps ((ast sym))
  ast)




