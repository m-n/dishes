DISHES
======

Some simple read macros.

These are implemented as functions independent of the macro characters
to which they might be bound. The library is called DISHES because a
convenient way to serve this style of read macro is by using
LAZY-SUSAN.

Favorites
---------

`comment-lines-suppress-forms` is like `#+(or)`, but also treats the
current line as a comment. I ususally bind it to `#\# #\;`. For
example, if you want to comment out some code while you reimplement
it:

    (defun plus (a b)
      ;; work in progress using the macro as a reference
      )

    #; reimplementing as function
    (defmacro plus (a b)
      `(+ ,a ,b))

`repl-run-program-reader` does `run-program` and prints the
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
    MAKE-CUSTOMIZABLE-;-READER:   Return a comment reader with customizeable behavior for different ; counts.
    REPL-RUN-PROGRAM-READER: Interpret the rest of the line as a shell command. Print output.

Read until what?
----------------

For reader macros which process the stream character by character or
read a list of elements instead of reading a single element or a line,
it is necessary for us to define a character that determines when the
input to the macro is complete. We determine the ending character
based on the starting character using `closer`. For `#\(` the closer
is `#\)`. Likewise for `#\{` and `#\}`; `#\[` and `#\]`; and `#\<` and
`#\>`. For other characters the closer is the character itself, so if
you bind one of these functions to the dispatch macro character `#\#
#\"`, then `#\"` will be used as the ending character.

For macros which internally read a list, it is likely necessary to set
the closer's syntax to the close parenthesis:

    (set-syntax-from-char #\] #\) *my-readtable*)

Otherwise in `[foo bar]` the reader will include the closing brace in
the symbol name, `"BAR]"`, instead of recognizing it as the completion
of the read macro's input.
