(defpackage :impl
    (:use
        #:cl
        #:node
    )
    (:export
        #:x
    )
)

(in-package :impl)

(defun x ()
    1
)