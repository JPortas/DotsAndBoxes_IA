(defpackage :node
    (:use
        #:COMMON-LISP
    )
    (:export
        #:hello
        #:ups
    )
)

(in-package :node)
 
(defun hello (name)
    (format T "Hello ~d!" name)
)

(defun ups (name)
    (format T "Hello ~d!" name)
)