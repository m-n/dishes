;;;; package.lisp

(defpackage #:dishes
  (:use #:cl)
  (:export
   #:not-reader
   #:declare-reader
   #:lambda-apply-reader
   #:thunk-reader
   #:\)-reader
   #:default-syntax-reader
   #:comment-line-suppress-forms
   #:*print-commented-forms*
   #:unescapable-string-reader
   #:interned-string-reader
   #:html-reader
   #:pathname-this-directory-reader
   #:run-time-symbol-reader
   #:closer
   ))
