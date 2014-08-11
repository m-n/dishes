;;;; dishes.asd

(asdf:defsystem #:dishes
  :serial t
  :description "Some simple reader macros."
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on ()
  :components ((:file "package")
               (:file "utils")
               (:file "dishes")))

(asdf:defsystem #:dishes-test
  :serial t
  :depends-on (:dishes :yarty)
  :components ((:file "tests")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :dishes))))
  (asdf:operate 'asdf:load-op :dishes-test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :yarty))
           :dishes-test))
