;;;; allegro-test.asd

(asdf:defsystem #:allegro-test
  :description "Test for Allegro 5 Common Lisp Wrapper"
  :author "Ryan Burnside"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-liballegro ; https://github.com/resttime/cl-liballegro
	       #:alexandria)
  :components ((:file "package")
	       (:file "additional-functions")
               (:file "allegro-test")))
