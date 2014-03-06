;;;; dishes.lisp

(in-package #:dishes)

(defvar *print-commented-forms* nil
  "Print what when a comment-line-supress-forms reader macro is triggerd?
  
  Options are :COMMENT or T     -- the comment line
              NIL or :NONE      -- don't print
              :FORM             -- bind *READ-SUPRESS* to nil, print read forms.")

(defun comment-line-suppress-forms (stream char &optional count)
  "Comments the rest of the line then read-suppresses COUNT forms, or 1 form.

Rationale: #+(or) is seen by many to be cryptic, and #+(or) is more
useful when accompanied by an explanatory comment."
  (declare (ignorable char))
  (cond ((member *print-commented-forms* '(t :comment :form))
	 (format t "~&~%#~@[~A~]~A~A" count char (read-line stream t nil t)))
	(t (do () ((char= (read-char stream nil #\Newline t) #\Newline)))))
  (cond ((eq *print-commented-forms* :form)
	 (let ((*read-suppress* nil))
	   (loop repeat (or count 1) do (print (read stream t nil t)))
	   (values)))
	(t (let ((*read-suppress* t))
	     (loop repeat (or count 1) do (read stream t nil t))
	     (values)))))

(defun pathname-this-directory-reader (stream char &optional count)
  "Read form and return a pathname in the files directory.

Assumes the form is a backslash escaped form ending at the first
unescaped (closer char)."
  (declare (ignore count))
  (let ((end-char (closer char)))
    (merge-pathnames
     (with-output-to-string (name.type)
       (loop for c = (read-char stream t nil t)
             until (char= c end-char)
             do (write-char (if (char= c #\\)
                                (read-char stream t nil t)
                                c)
                            name.type)))
     (or *compile-file-truename*
         *load-truename*
         *default-pathname-defaults*))))

(defun run-time-symbol-reader (stream char &optional count)
  "Reads a delimited list into a form that will find-symbol at runtime."
  (declare (ignore count))
  (destructuring-bind (package name) (read-delimited-list (closer char) stream)
    `(find-symbol ,(symbol-name name) ,(symbol-name package))))

(defun make-interned-string-reader (&key case-insensitive-p)
  (let ((words (make-hash-table :test (if case-insensitive-p 'equalp 'equal))))
    (lambda (stream char &optional count)
      (declare (ignore count))
      (if stream
          (let* ((string (with-output-to-string (s)
                           (loop for c = (read-char stream t)
                                 until (char= c (closer char))
                                 do (princ (if (char= c #\\)
                                               (read-char stream t)
                                               c)
                                           s))))
                 (interned-string (gethash string words)))
            (if interned-string
                interned-string
                (setf (gethash string words) string)))
          words))))

(setf (symbol-function 'interned-string-reader)
      (make-interned-string-reader :case-insensitive-p nil))

(setf
 (documentation 'interned-string-reader 'function)
 "A string reader which interns the strings.

Backslash is assumed to be the only single escape, and the end of the
string is the first unescaped (closer char)")

(defun unescapable-string-reader (stream char &optional count)
  "Accumulate each character into a string until (closer char) is seen."
  (declare (ignorable count))
  (with-output-to-string (string)
    (do ((c (read-char stream) (read-char stream)))
	((char-equal c (closer char)))
      (write-char c string))))

(setf (fdefinition 'uninterned-symbol-reader)
      (lambda (stream char &optional count)
        (funcall (get-dispatch-macro-character #\# #\: *standard-readtable*)
                 stream
                 char
                 count)))

(setf (documentation 'uninterned-symbol-reader 'function)
      "Read the following characters as an uninterned symbol.

Doesn't include the dispatch character.")

(defun formatted-string-reader (stream char &optional count)
  "Returns the value of (format nil string)."
  (declare (ignore count))
  (let ((string (with-output-to-string (s)
                  (loop for c = (read-char stream t)
                        until (char= c (closer char))
                        do (princ (if (char= c #\\)
                                      (read-char stream t)
                                      c)
                                  s)))))
    (format nil string)))

(defun not-reader (stream char &optional count)
  "Like ' but for (not ...) instead of (quote ...)"
  (declare (ignore count char))
  `(not ,(read stream t nil t)))

(defun declare-reader (stream char &optional count)
  "Like ' but for (declare ...) instead of (quote ...)"
  (declare (ignore count char))
  `(declare ,(read stream t nil t)))

(defun lambda-apply-reader (stream char &optional count)
  "Read next form as (lambda (&rest args) (apply form args))

This could be used to allow using macros that expand to lambdas in car
position."
  (declare (ignore count char))
  `(lambda (&rest #1=#:args) (apply ,(read stream t nil t) #1#)))

(defun thunk-reader (stream char &optional count)
  "Read next form as the body of a lambda with an ignored &rest lambda list."
  (declare (ignore count char))
  `(lambda (&rest #1=#:args)
     (declare (ignore #1#))
     ,(read stream t nil t)))

(defun \)-reader (&rest args)
  "The reader macro function for close paren in the standard readtable."
  (apply (get-macro-character #\) ()) args))

(defun default-syntax-reader (stream char &optional count)
  "Read the next form with *readtable* bound to the standard readtable."
  (declare (ignore count char))
  (let ((*readtable* *standard-readtable*))
    (read stream t nil t)))

(defparameter *html-solo-tags* '("input" "br"))

(defun html-reader (stream char &optional count)
  "Experimental reader for reading sexps into html."
  (declare (ignore count))
  (let ((tagname (make-array '(0) :fill-pointer 0 :adjustable t
			     :element-type 'base-char))
	(end-delimiter (closer char)))
    (with-output-to-string (tag tagname)
      (loop until (member (peek-char nil stream t nil t)
                          `(#\Tab #\Newline #\Space ,end-delimiter))
            do (princ (read-char stream t nil t) tag)))
    (flet ((opener (tagname)
	     (if (char= #\( (peek-char t stream t nil t))
		 `(mkstr #\< ,tagname
                         ,@(loop for (k v) on (read stream) by #'cddr
                                 collect `(mkstr " " ,(string-downcase (symbol-name k))
                                                 "=\"" ,v #\"))
                         #\>)
		 (mkstr #\< tagname #\>))))
      (if (member tagname *html-solo-tags* :test #'string-equal)
	  (prog1 `(mkstr ,(opener tagname))
            (read-char stream nil nil t))
	  `(mkstr
	    ,(opener tagname)
	    ,@(read-delimited-list end-delimiter stream t)
	    ,(concatenate 'string "</" tagname ">"))))))
