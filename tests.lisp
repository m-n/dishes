
(defpackage #:dishes-test
  (:use #:cl #:yarty))

(in-package #:dishes-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* (copy-readtable ()))
  (set-dispatch-macro-character
   #\# #\[ #'dishes:pathname-this-directory-reader))

(defvar *test-readtable* (copy-readtable ()))

(defmacro define-reader-test (chars function-name &body body)
  `(flet ((setup-readtable ()
            (if (= (length ',chars) 1)
                (set-macro-character (first ',chars) (function ,function-name) t
                                     *test-readtable*)
                (set-dispatch-macro-character (first ',chars) (second ',chars)
                                              (function ,function-name)
                                              *test-readtable*))))
     (deftest ,(intern (symbol-name function-name)
                       "DISHES-TEST")
       (setup-readtable)
       ,@body)))

(defun test-read (string)
  (let ((*readtable* *test-readtable*)
        (*package* (find-package "DISHES-TEST")))
    (read-from-string string)))

(defun test-read/suppress1 (string)
  (test-read (concatenate 'string
                          "#+(or) "
                          string)))

(define-reader-test (#\# #\;) dishes:comment-line-suppress-forms
  (each
    (= 1
       (test-read "#; foo
foo
1"))
    (= 2
       (test-read/suppress1 "#; foo
foo
1
2
"))
    (= 2
       (test-read "#; foo
#+(or)
foo
1
2
"))))

(define-reader-test (#\^) dishes:declare-reader
  (each
    (equalp '(declare (ignore foo))
            (test-read "^(ignore foo)"))
    (= 4
       (test-read/suppress1 "^(ignore undefined-package%%:foo) 4"))))

(define-reader-test (#\^) dishes:default-syntax-reader
  (each
    (equalp '(foo ^bar)
            (test-read "^(foo ^bar)"))
    (eq t (test-read/suppress1 "^(foo ^bar undefined-package%%:bar) t"))))

(define-reader-test (#\# #\") dishes:formatted-string-reader
  (each
    (position #\Newline "foo ~
bar")
    (not (position #\Newline (test-read "#\"bar ~
foo\"")))
    (eq (test-read/suppress1 "#\"bar\" foo")
        'foo)))

(define-reader-test (#\") dishes:interned-string-reader
  (each
    (eq (test-read "\"foo\"") (test-read "\"foo\""))
    (eq t (test-read/suppress1 "\"foo\" t"))))

(define-reader-test (#\^) dishes:lambda-apply-reader
  (each
    (eq (car (test-read "^(foo)"))
        'lambda)
    (eq (test-read/suppress1 "^(foo) bar")
        'bar)))

(define-reader-test (#\!) dishes:not-reader
  (each
    (equalp (test-read "!foo")
            '(not foo))
    (equalp (test-read/suppress1 "!undefined-package%%:bar foo")
            'foo)))

(define-reader-test (#\# #\[) dishes:pathname-this-directory-reader
  (each
    (equal (namestring (asdf/system:system-relative-pathname :dishes "bar"))
           ;; This read macro must run during read time for the right
           ;; *compile-file-truename* or *load-truename* to be active.
           (namestring #[bar]))
    (eq (test-read/suppress1 "#[bar] foo")
        'foo)))

(define-reader-test (#\# #\Space) dishes:repl-run-program-reader
  (each
    (test-read "# ls")
    (eq (test-read/suppress1 "# ls
bar")
        'bar)))

(set-syntax-from-char #\] #\) *test-readtable*)

(define-reader-test (#\[) dishes:run-time-symbol-reader
  (each
    (eq (eval (test-read "[dishes run-time-symbol-reader]"))
        'dishes:run-time-symbol-reader)
    (not (eq (test-read "[dishes run-time-symbol-reader]")
             'dishes:run-time-symbol-reader))
    (eq (test-read/suppress1 "[undefined-packag%% foo] bar")
        'bar)))

(define-reader-test (#\^) dishes:thunk-reader
  (each
    (equal (car (test-read "^(progn (print 'baz))"))
           'lambda)
    (equal (test-read/suppress1 "^(progn (print 'baz)) foo")
           'foo)))

(define-reader-test (#\/) dishes:unescapable-string-reader
  (each
    (string-equal (test-read "/\\bar\\/")
                  (read-from-string "\"\\\\bar\\\\\""))
    (eq (test-read/suppress1 "/bar/ foo")
        'foo)))

(define-reader-test (#\$) dishes:uninterned-symbol-reader
  (each
    (not (eq (test-read "$bar")
             (test-read "$bar")))
    (string= (test-read "$bar")
             'bar)
    (eq (test-read/suppress1 "$bar foo")
        'foo)))
