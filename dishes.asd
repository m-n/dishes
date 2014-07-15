;;;; dishes.asd

(asdf:defsystem #:dishes
  :serial t
  :description "Some simple read macros"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:uiop
               )
  :components ((:file "package")
               (:file "utils")
               (:file "dishes")))
