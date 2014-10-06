;;;; package.lisp

(defpackage #:lambda-mta
  (:use #:cl)
  (:export #:compile-to-file
           #:compile-to-binary
           #:compile-and-execute
           #:generate-binary))
