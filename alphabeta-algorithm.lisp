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
    (- (funcall 'get-p1-closed-boxes node) (funcall 'get-p2-closed-boxes node))
)

(defun alphabeta-algorithm(node)
    (funcall 'alphabeta node 20 most-negative-fixnum most-positive-fixnum T)
)

(defun max-value (v1 v2)
"Retorna o maior valor entre dois números"
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

(defun min-value (v1 v2)
"Retorna o menor valor entre dois numeros"
    (cond
        (
            (< v1 v2)
            v1
        )
        (
            (> v1 v2)
            v2
        )
        (T
            v2
        )
    )
)

(defun node-max (v sucessores alfa beta depth maximazing-player)
"Percorre a lista de sucessores que seria o max e retorna a melhor avaliação de baixo para cima."
    (cond
        (
            (null sucessores)
            v
        )
        (T
            (let
                (
                    (a (max-value alfa (max-value v (funcall 'alphabeta (car sucessores) depth alfa beta NIL))))
                )
                (cond
                    (
                        (<= beta a)
                        (max-value v (funcall 'alphabeta (car sucessores) depth alfa beta NIL))
                    )
                    (T
                        (node-max 
                            (max-value v (funcall 'alphabeta (car sucessores) depth alfa beta NIL))
                            (cdr sucessores)
                            a
                            beta
                            (- depth 1)
                            maximazing-player
                        )
                    )
                )
            )
        )
    )
    #|(cond
        (
            (null sucessores)
            v
        )
        (
            (<= beta (max-value alfa (max-value v (funcall 'alphabeta (car sucessores) alfa beta depth NIL))))
            (max-value v (funcall 'alphabeta (car sucessores) alfa beta depth NIL))
        )
        (T
            (node-max 
                (max-value v (funcall 'alphabeta (car sucessores) alfa beta depth NIL))
                (cdr sucessores)
                (max-value alfa (max-value v (funcall 'alphabeta (car sucessores) alfa beta depth NIL)))
                beta
                depth
                maximazing-player
            )
        )
    )|#
)

(defun node-min (v sucessores alfa beta depth maximazing-player)
"Percorre a lista de sucessores que seria o min e retorna a melhor avaliação de baixo para cima."
    (cond
        (
            (null sucessores)
            v
        )
        (T
            (let
                (
                    (b (min-value beta (min-value v (funcall 'alphabeta (car sucessores) depth alfa beta T))))
                )
                (cond
                    (
                        (<= b alfa)
                        (min-value v (funcall 'alphabeta (car sucessores) depth alfa beta T))
                    )
                    (T
                        (node-min
                            (min-value v (funcall 'alphabeta (car sucessores) depth alfa beta T))
                            (cdr sucessores)
                            alfa
                            b
                            (- depth 1)
                            maximazing-player
                        )
                    )
                )
            )
        )
    )
    #|(cond
        (
            (null sucessores)
            v
        )
        (
            (<= (min-value beta (min-value v (funcall 'alphabeta (car sucessores) alfa beta depth T))) alfa)
            (min-value v (funcall 'alphabeta (car sucessores) alfa beta depth T))
        )
        (T
            (node-min
                (min-value v (funcall 'alphabeta (car sucessores) alfa beta depth T))
                (cdr sucessores)
                alfa
                (min-value beta (min-value v (funcall 'alphabeta (car sucessores) alfa beta depth T)))
                depth
                maximazing-player
            )
        )
    )|#
)

(defun alphabeta (node depth alpha beta maximazing-player)
    (cond
        (
            (<= depth 0)
            (eval-state node)
        )
        (T
            (cond
                (
                    maximazing-player
                    (let
                        (
                            (v most-negative-fixnum)
                            (successors (funcall 'get-successors node 1))
                        )
                        (cond
                            (
                                (not (null successors))
                                (node-max
                                    v
                                    successors
                                    alpha
                                    beta
                                    (- depth 1)
                                    maximazing-player
                                )
                            )
                            (T
                                (eval-state node)
                            )
                        )
                    )
                )
                (T
                    (let
                        (
                            (v most-positive-fixnum)
                            (successors (funcall 'get-successors node 2))
                        )
                        (cond
                            (
                                (not (null successors))
                                (node-min
                                    v
                                    successors
                                    alpha
                                    beta
                                    (- depth 1)
                                    maximazing-player
                                )
                            )
                            (T
                                (eval-state node)
                            )
                        )
                    )
                )
            )
        )
    )
)
