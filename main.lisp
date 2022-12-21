(load "board.lisp")
(load "node.lisp")

(load "not-informed-search.lisp")
(load "informed-search.lisp")

(load "example-values.lisp")
(load "unit-tests.lisp")

(defpackage #:main
    (:use
        #:COMMON-LISP
        #:COMMON-LISP-USER
        #:board
        #:node
        #:informed-search
        #:not-informed-search
        #:example-values
        #:unit-tests
    )
    (:import-from
        #:COMMON-LISP-USER #:exit
    )
    (:export
        #:main
        #:load-problems
        #:read-lists-from-string
        #:exit
    )
)

(in-package :main)

;execute
;sbcl --load .\main.lisp --eval '(in-package #:main)' --eval '(main)'
;to start your jorney or
;sbcl --load .\main.lisp --eval '(in-package #:main)'
;to teste by your own

(defun create-list-from-file (filename)
    (cond
        (
            (not (stringp filename))
            nil
        )
        (t  
            (with-open-file (stream filename)
                (let ((line (read-line stream)))
                    (with-input-from-string (s (string line))
                        (loop
                            for line = (read s nil)
                            while line
                            collect (cond
                                (
                                    (eql line '/)
                                    (progn nil)
                                )
                                (t line)
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun load-problems (filename)
    (cond
        (
            (not (stringp filename))
            nil
        )
        (T
            (apply #'append (create-list-from-file filename))
        )
    )
)

(defun main ()
    (format T "Welcome to IA 0.1 :)")
)

