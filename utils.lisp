;;;; dishes/utils.lisp

(in-package #:dishes)

(defun closer (char)
  "The matching end character to char.

 (case char
    (#\( #\))
    (#\{ #\})
    (#\[ #\])
    (#\< #\>)
    (t char))"
  (case char
    (#\( #\))
    (#\{ #\})
    (#\[ #\])
    (#\< #\>)
    (t char)))

(defun mkstr (&rest args)
  "Concatenates args printed into string.
  From On Lisp."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun mkstr% (&rest args)
  "A clone of mkstr without a compiler macro."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(define-compiler-macro mkstr (&whole w &environment env &rest rest)
  (declare (ignorable env w))
  (unless rest (return-from mkstr ""))
  (let (unnested-args)
    (labels ((walker (form)
               (cond ((and (listp form) (eql (car form) 'mkstr))
                      (mapc #'walker (cdr form)))
                     (t
                      (push form unnested-args)))))
      (mapc #'walker rest)
      (setq unnested-args (reverse unnested-args))
      (let (coalesced-args
            coalescing-args)
        (dolist (arg unnested-args)
          (cond ((or (stringp arg) (characterp arg))
                 (push arg coalescing-args))
                (t (when coalescing-args
                     (push (apply #'mkstr (reverse coalescing-args)) coalesced-args)
                     (setq coalescing-args nil))
                   (push arg coalesced-args))))
        (when coalescing-args
          (push (apply #'mkstr
                       (reverse coalescing-args))
                coalesced-args))
        (if (> (length coalesced-args) 1)
            ;; returning (mkstr ...) seems to cause infinite recursions
            `(mkstr% ,@(reverse coalesced-args))
            (car coalesced-args))))))

(defvar *standard-readtable* (load-time-value (copy-readtable nil)))
