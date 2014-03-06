DISHES
======

Some simple read macros.

These are implemented as functions independent of the macro characters
to which they might be bound. The library is called DISHES because a
convenient way to serve this style of read macro is by using
LAZY-SUSAN.

Example
-------

Libraries are less likely to clash with other libraries if they only
alter readtables which they themselves created.

    ;; Set up in an eval-when or a separate file
    (defvar *my-readtable* (copy-readtable ()))

    (set-dispatch-macro-character
     #\# #\; #'comment-line-suppress-forms *my-readtable*)

Then, use the readtable in another file using an eval-when:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setq *readtable* *my-readtable*))

Alternatively, you can use this system in conjuntion with LAZY-SUSAN.

    ;; The setup
    (ls:setup-package-rt (my-package)
      (#\# #\;) #'comment-line-suppress-forms)

That will setup and set the readtable in the current file. By default
this uses a copy of the LAZY-SUSAN readtable as a base for your
particular bindings. To use it in another file:

    (ls:in-package/rt #:my-package)

or

    (in-package #:my-package)

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setq *readtable* (ls:package-rt '#:my-package)))

The Dishes
==========

    COMMENT-LINE-SUPPRESS-FORMS:  Comments the rest of the line then read-suppresses COUNT forms, or 1 form.
    *PRINT-COMMENTED-FORMS*: Print what when a comment-line-supress-forms reader macro is triggerd?
    PATHNAME-THIS-DIRECTORY-READER:    Read form and return a pathname in the files directory.
    RUN-TIME-SYMBOL-READER:  Reads a delimited list into a form that will find-symbol at runtime.
    UNINTERNED-SYMBOL-READER:     Read the following characters as an uninterned symbol.
    INTERNED-STRING-READER:  A string reader which interns the strings.
    FORMATTED-STRING-READER: Returns the value of (format nil string).
    UNESCAPABLE-STRING-READER:    Accumulate each character into a string until (closer char) is seen.
    NOT-READER:         Like ' but for (not ...) instead of (quote ...)
    DECLARE-READER:     Like ' but for (declare ...) instead of (quote ...)
    LAMBDA-APPLY-READER:     Read next form as (lambda (&rest args) (apply form args))
    THUNK-READER:       Read next form as the body of a lambda with an ignored &rest lambda list.
    )-READER:           The reader macro function for close paren in the standard readtable.
    DEFAULT-SYNTAX-READER:   Read the next form with *readtable* bound to the standard readtable.
    HTML-READER:        Experimental reader for reading sexps into html.
    CLOSER:             The matching end character to char.
    CUSTOMIZABLE-;-READER:   Make comment behavior customizable by variables.
        *1;* *2;* *3;* *4+;*



Read until what?
----------------

For reader macros which process the characters on the stream instead
of recursively calling read, it's necessary for us to define a
character that determines when the input to the macro is
complete. Since these macros are designed to be bound to arbitrary
characters, we determine the ending character based on the starting
character rather than on the function. We use CLOSER to determine
this; for (, {, [, <, it matches the closing >, ], }, ). For other
characters the closer is the character itself, so if you bind one of
these functions to the dispatch macro character #", then " will be
used as the ending character.
