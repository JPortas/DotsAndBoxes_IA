(load "example-values.lisp")
(load "board.lisp")
(load "node.lisp")

(load "unit-tests.lisp")

(defpackage #:main
    (:use
        #:COMMON-LISP
        #:COMMON-LISP-USER
        #:example-values
        #:board
        #:node
        #:unit-tests
    )
    (:import-from
        #:COMMON-LISP-USER #:exit
    )
    (:export
        #:main
        #:exit
    )
)

(in-package :main)

;execute
;sbcl --load .\main.lisp --eval '(in-package #:main)' --eval '(main)'
;to start your jorney or
;sbcl --load .\main.lisp --eval '(in-package #:main)'
;to teste by your own
(defun main ()
    (format T "Welcome to IA 0.1 :)")
)

