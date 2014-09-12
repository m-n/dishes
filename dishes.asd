;;;; dishes.asd

(asdf:defsystem #:dishes
  :serial t
  :description "Some simple reader macros."
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on ()
  :components ((:file "package")
               (:file "utils")
               (:file "dishes"))
  :in-order-to ((test-op (load-op dishes-test)))
  :perform (test-op :around (op c)
                    (call-next-method)
                    (funcall (intern (symbol-name :run-tests)
                                     :yarty)
                             :dishes-test)))

(asdf:defsystem #:dishes-test
  :serial t
  :depends-on (:dishes :yarty)
  :components ((:file "tests")))
