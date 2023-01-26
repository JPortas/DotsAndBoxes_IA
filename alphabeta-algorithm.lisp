(defpackage :alphabeta-algorithm
    (:use
        #:COMMON-LISP
        #:node
    )
    (:export
        #:alphabeta
        #:alphabeta-algorithm
        #:eval-state
        #:max-value
    )
)

(in-package :alphabeta-algorithm)

(defun eval-state (node)
    (format T "~d" node)
    (- (funcall 'get-p1-closed-boxes node) (funcall 'get-p2-closed-boxes node))
)

(defun alphabeta-algorithm(node)
    (alphabeta node 100 most-negative-fixnum most-positive-fixnum T)
)

(defun max-value (v1 v2)
    (cond
        (
            (> v1 v2)
            v1
        )
        (
            (< v1 v2)
            v2
        )
        (T
            v1
        )
    )
)

(defun alphabeta (node depth alpha beta maximazing-player)
    (cond
        (
            (= depth 0)
            (eval-state node)
        )
        (T
            (cond
                (
                    maximazing-player
                    (let
                        (
                            (v most-negative-fixnum)
                            (childrens (funcall 'get-successors node 1))
                        )
                        (reduce
                            (lambda (alpha child depth beta maximazing-player)
                                (let*
                                    (
                                        (v (alphabeta child depth alpha beta NIL))
                                        (alp (max-value v alpha))
                                    )
                                    (if
                                        (<= beta alpha)
                                        (values v)
                                        child
                                    )
                                )
                            )
                            childrens
                            :initial-value (list depth alpha beta maximazing-player)
                        )

                    )
                )
                (T
                    0
                )
            )
        )
    )
    #|(let 
        (
            (l1 (funcall 'get-successors node 1))
        )
        (reduce
            (lambda (value x)
                (cond 
                    (
                        (> x 10)
                        (values t)
                    )
                    (t val)
                )
            )
          l1
          :initial-value nil
        )
    )|#
    
    
    
    #|(cond
        (
            (= depth 0)
            (eval-state node)
        )
        (T
            (cond
                (
                    maximazing-player
                    (let
                        (
                            (successors (funcall 'get-successors node 1))
                        )
                    )
                )
                (T

                )
            )
        )
    )|#
)


#|(defun illegal-repeat-instruction (nodes-list depth alpha beta maximazing-player)
    (cond
        (
            (not (null nodes-list))
            (let
            
            )
            (alphabeta)
            (illegal-repeat-instruction (cdr nodes-list) depth alpha maximazing-player)
        )
        (T
            0
        )
    )
)|#

#|(defun add-to-open-list-dfs (OPEN-LIST successors-list)
    (cond
        (
            (and (null OPEN-LIST) (null successors-list))
            NIL
        )
        (
            (null OPEN-LIST)
            successors-list
        )
        (
            (null successors-list)
            OPEN-LIST
        )
        (T
            (append successors-list OPEN-LIST)
        )
    )
)

(defun max-value (v1 v2)
    (cond
        (
            (> v1 v2)
            v1
        )
        (
            (< v1 v2)
            v2
        )
        (T
            T
        )
    )
)

(defun illegal-repeat-instruction (nodes-list alpha beta)
    (let

    )
)

(defun alphabeta (node depth alpha beta maximazing-player &optional (open-list '()))

    (cond
        (
            (= depth 0)
            (eval-state node)
        )
        (T
            (cond
                (
                    maximazing-player
                    (let*
                        (
                            (max-eval -999999)
                            (successors (funcall 'get-successors node 1))
                            (eva
                                (cond
                                    (
                                        (not (null successors))
                                        (alphabeta
                                            (car successors)
                                            (- depth 1)
                                            alpha
                                            beta
                                            NIL
                                            (add-to-open-list-dfs open-list (cdr successors))
                                        )
                                    )
                                    (T
                                        (eval-state node)
                                    )
                                )
                            )
                            (max (max-value max-eval eva)))
                            (alp (max-value alpha ))
                        )
                        eva
                    )
                )
                (T
                    (let*
                        (
                            (min-eval 9999999)
                            (successors (funcall 'get-successors node 2))
                            (eva
                                (cond
                                    (
                                        (not (null successors))
                                        (alphabeta
                                            (car successors)
                                            (- depth 1)
                                            alpha
                                            beta
                                            T
                                            (add-to-open-list-dfs open-list (cdr successors))
                                        )
                                    )
                                    (T
                                        (eval-state node)
                                    )
                                )
                            )
                            (max-eval eva)
                            ()
                        )
                        (format T "~%~d~%" node)
                        eva
                    )
                )
            )
        )
) |#




