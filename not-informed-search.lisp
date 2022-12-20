(defpackage :not-informed-search
    (:use
        #:COMMON-LISP
        #:node
    )
    (:export
        #:dfs-start
    )
)

(in-package :not-informed-search)

;(dfs-start (ex-node))
(defun dfs-start (start-node)
    (get-successors start-node)
)