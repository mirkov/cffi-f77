;;;; cffi+f77.asd

(asdf:defsystem #:cffi+f77
  :serial t
  :depends-on (#:cffi
               #:lisp-unit)
  :components (;; define package
	       (:file "package")
	       ;; loading and unloading the library
	       (:file "library-ops")
	       ;; one simple example
               (:file "simple-example")
	       ;; examples of passing various types (logical, real,
	       ;; ...) to f77
	       (:file "subroutine-argument-passing-tests")))

