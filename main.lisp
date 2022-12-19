(load "node.lisp")
(load "impl.lisp")

(defpackage #:main
    (:use
        #:COMMON-LISP
        #:COMMON-LISP-USER
        #:node
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


(defun main ()
    (format T "Heyo World!")
)

