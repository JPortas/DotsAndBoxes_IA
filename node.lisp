(defpackage :node
    (:use
        #:COMMON-LISP
    )
    (:export
        #:new-successor
        #:get-node-state
    )
)

(in-package :node)

(defun new-successor (board depth parent &optional heuristic)
    (cond
        (
            (null heuristic)
            (list board depth parent)
        )
        (T
            (list board depth heuristic parent)
        )
    )
)

(defun get-node-state (node)
    (cond
        (
            (null node)
            NIL
        )
        (
            (car node)
        )
    )
)