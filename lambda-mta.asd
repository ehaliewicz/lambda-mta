;;;; lambda-mta.asd

(asdf:defsystem #:lambda-mta
  :serial t
  :description "Describe lambda-mta here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:trivial-shell)
  :components ((:file "package")
               (:file "ast")
               (:file "cps")
               (:file "closure")
               (:file "codegen")
               (:file "shell")))

