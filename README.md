DISHES
======

Some simple reader macros.

DISHES exports reader macro functions suitable for binding to abitrary
macro or dispatch macro characters. The library is called DISHES
because a convenient way to serve this style of reader macro is by
using my LAZY-SUSAN library. They are also convenient to use with the
more popular NAMED-READTABLES.

Favorites
---------

`comment-lines-suppress-forms` is like `#+(or)`, but also treats the
current line as a comment. I ususally bind it to `#\# #\;`. For
example, if you want to comment out some code while you reimplement
it:

    #; reimplementing as function
    (defmacro plus (a b)
      `(+ ,a ,b))

    (defun plus (a b)
      ;; work in progress using the macro as a reference
      )

`repl-run-program-reader` does `uiop:run-program` and prints the
output. Suppose you bind it to `#\# #\Space`, then you might have the
following interaction with your lisp repl:

    DISHES> # git status
    On branch master
    Changes not staged for commit:
      (use "git add <file>..." to update what will be committed)
      (use "git checkout -- <file>..." to discard changes in working directory)

	    modified:   README.md

    no changes added to commit (use "git add" and/or "git commit -a")
    0
    DISHES>

`pathname-this-directory-reader` reads a pathname relative to the file
being compiled, loaded, or to the default pathname defaults, in that
order of preference. So if you bind it to `#\# #\[`:

    ;;;; in /home/me/quicklisp/local-projects/dishes/test.lisp
    #[bar/foo.lisp]
    ;; reads as #P"/home/me/quicklisp/local-projects/dishes/bar/foo.lisp"

Note: The path will only be updated when the file is recompiled.

Example Readtable Use
=====================

Below are three separate ways to setup and use a readtable with DISHES
syntax added. Both the NAMED-READTABLES and LAZY-SUSAN method take
care of enabling the syntax within SLIME's `C-c C-c` command -- and at
the slime repl if you switch to the new package -- but the CL
primitives method does not.

The LAZY-SUSAN method also enables its readtable adjustments which
still have some gotchyas, so I might recommend to use
NAMED-READTABLES.

Using NAMED-READTABLES
----------------------

To create and setup the readtable:

    ;;;; syntax.lisp

    (defpackage #:example-package (:use :cl))

    (in-package #:example-package)

    (named-readtables:defreadtable served
      (:merge :standard)
      (:dispatch-macro-char #\# #\; #'dishes:comment-line-suppress-forms))

To use the readtable:

    ;;;; awesome.lisp

    (in-package #:example-package)

    (named-readtables:in-readtable served)

Using LAZY-SUSAN
----------------

To create and setup the readtable:

    ;;;; syntax.lisp

    (defpackage #:example-package (:use :cl))

    (in-package #:example-package)

    (ls:setup-package-rt (example-package)
      (#\# #\;) #'dishes:comment-line-suppress-forms)

To use the readtable:

    ;;;; awesome.lisp

    (in-package #:example-package)

    (ls:in-package/rt #:example-package)

Using CL Primitives
-------------------

To create and setup the readtable:

    ;;;; syntax.lisp

    (defpackage #:example-package (:use :cl))

    (in-package #:example-package)

    (defvar *served-readtable* (copy-readtable ()))

    (set-dispatch-macro-character
     #\# #\; #'dishes:comment-line-suppress-forms *served-readtable*)

To use the readtable:

    ;;;; awesome.lisp

    (in-package #:example-package)

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setq *readtable* *served-readtable*))

The Dishes
==========

    COMMENT-LINE-SUPPRESS-FORMS:  Comments the rest of the line then read-suppresses COUNT forms, or 1 form.
    *PRINT-COMMENTED-FORMS*: Print what when a comment-line-suppress-forms reader macro is triggerd?
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
    HTML-READER:        Experimental reader for reading s-exps into html.
    CLOSER:             The matching end character to char.
    MAKE-CUSTOMIZABLE-\;-READER:   Return a comment reader with customizeable behavior for different ; counts.
    REPL-RUN-PROGRAM-READER: Interpret the rest of the line as a shell command. Print output.
    HASH-TABLE-READER:  Read into a form which will create a hash table when evaluated.

Read until what?
----------------

For reader macros which process the stream character by character or
read a list of elements -- instead of reading a single element or a
line -- it is necessary for us to define a character that determines
when the input to the reader macro is complete. We determine the
ending character based on the starting character using `closer`. For
`#\(` the closer is `#\)`. Likewise for `#\{` and `#\}`; `#\[` and
`#\]`; and `#\<` and `#\>`. For other characters the closer is the
character itself, so if you bind one of these functions to the
dispatch macro character `#\# #\"`, then `#\"` will be used as the
ending character.

For macros which internally read a list, it is likely necessary to set
the closer's syntax to the close parenthesis:

    (set-syntax-from-char #\] #\) *my-readtable*)

Otherwise in `[foo bar]` the reader will include the closing brace in
the symbol name, `"BAR]"`, instead of recognizing it as the completion
of the reader macro's input.
