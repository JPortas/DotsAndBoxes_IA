(defpackage :informed-search
    (:use
        #:COMMON-LISP
        #:node
    )
    (:export
        ;#:A-star
    )
)

(in-package :informed-search)

;(defun A-star (start-node closed-boxes-goal) 
;    (funcall 'get-successors start-node (symbol-function 'heuristic-eval-by-remaining-to-close))
;)