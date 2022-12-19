(load "test-values.lisp")
(load "node.lisp")
(load "impl.lisp")

(defpackage #:main
    (:use
        #:COMMON-LISP
        #:COMMON-LISP-USER
        #:node
        #:test-values
        #:impl
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

