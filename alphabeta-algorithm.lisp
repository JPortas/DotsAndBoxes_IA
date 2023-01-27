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
        #:best-node-info
        #:best-node-value
        #:best-node-node
        #:player-numbers-handler
    )
)

(in-package :alphabeta-algorithm)

(defun best-node-info (v node)
    (list v node)
)

(defun best-node-value (best-node-info)
    (car best-node-info)
)

(defun best-node-node (best-node-info)
    (cadr best-node-info)
)

(defun eval-state (node player)
    (cond
        (
            (= player 2)
            (- (funcall 'get-p2-closed-boxes node) (funcall 'get-p1-closed-boxes node))
        )
        (T
            (- (funcall 'get-p1-closed-boxes node) (funcall 'get-p2-closed-boxes node))
        )
    )
)

(defun alphabeta-algorithm(node maximazing-player player)
    (funcall 'alphabeta node 15 most-negative-fixnum most-positive-fixnum maximazing-player player)
)

(defun bubble-aux (lista player)
    (cond
        (
            (null (cdr lista))
            lista
        )
        (
            (< (eval-state (car lista) player) (eval-state (cadr lista) player))
            (cons (car lista) (bubble-aux (cdr lista) player))
        )
        (t
            (cons (cadr lista) (bubble-aux (cons (car lista) (cddr lista)) player))
        )
    )
)

(defun bubble-sort (lista size player)
    (cond 
        (
            (null lista)
            NIL
        )
        (
            (null (cdr lista))
            lista
        )
        (
            (= size 1)
            (bubble-aux lista player)
        )
        (t
            (bubble-sort (bubble-aux lista player) (- size 1) player)
        )
    
    )
)

(defun successor-sort (successor player)
    (cond
        (
            (null successor)
            NIL
        )
        (T
            (bubble-sort successor (length successor) player)
        )
    )
)

(defun bubble-aux-dec (lista player)
    (cond
        (
            (null (cdr lista))
            lista
        )
        (
            (> (eval-state (car lista) player) (eval-state (cadr lista) player))
            (cons (car lista) (bubble-aux-dec (cdr lista) player))
        )
        (t
            (cons (cadr lista) (bubble-aux-dec (cons (car lista) (cddr lista)) player))
        )
    )
)

(defun bubble-sort-dec (lista size player)
    (cond 
        (
            (null lista)
            NIL
        )
        (
            (null (cdr lista))
            lista
        )
        (
            (= size 1)
            (bubble-aux-dec lista player)
        )
        (t
            (bubble-sort-dec (bubble-aux-dec lista player) (- size 1) player)
        )
    
    )
)

(defun successor-sort-dec (successor player)
    (cond
        (
            (null successor)
            NIL
        )
        (T
            (bubble-sort-dec successor (length successor) player)
        )
    )
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

(defun node-max (v sucessores alfa beta depth maximazing-player player)
"Percorre a lista de sucessores que seria o max e retorna a melhor avaliação de baixo para cima."
    (cond
        (
            (null sucessores)
            ;(best-node-info v (best-node-value best-node))
            v
        )
        (T
            (let*
                (
                    (v-value (best-node-value v))
                    (v-node (best-node-node v))
                    (ab (best-node-value (funcall 'alphabeta (car sucessores) depth alfa beta NIL player))) ;o ab seria o eval do no a baixo
                    (a (max-value alfa (max-value v-value ab)))
                )
                (cond
                    (
                        (<= beta a)
                        (best-node-info (max-value v-value ab) v-node)
                    )
                    (
                        (> a alfa)
                        (node-max
                            (best-node-info (max-value v-value ab) (car sucessores))
                                (cdr sucessores)
                                a
                                beta
                                (- depth 1)
                                maximazing-player
                                player
                        )
                    )
                    (T
                        (node-max 
                            (best-node-info (max-value v-value ab) v-node)
                            (cdr sucessores)
                            a
                            beta
                            (- depth 1)
                            maximazing-player
                            player
                        )
                    )
                )
            )
        )
    )
)

(defun node-min (v sucessores alfa beta depth maximazing-player player)
"Percorre a lista de sucessores que seria o min e retorna a melhor avaliação de baixo para cima."
    (cond
        (
            (null sucessores)
            v
            ;(best-node-info v (best-node-node best-node))
        )
        (T
            (let*
                (
                    (v-value (best-node-value v))
                    (v-node (best-node-node v))
                    (ba (best-node-value (funcall 'alphabeta (car sucessores) depth alfa beta T player))) ;o ab seria o eval do no a baixo
                    (b (min-value beta (min-value v-value ba)))
                )
                (cond
                    (
                        (<= b alfa)
                        (best-node-info (min-value v-value ba) v-node)
                    )
                    (
                        (< b beta)
                        (node-min
                            (best-node-info (min-value v-value ba) (car sucessores))
                            (cdr sucessores)
                            alfa
                            b
                            (- depth 1)
                            maximazing-player
                            player
                        )
                    )
                    (T
                        (node-min
                            (best-node-info (min-value v-value ba) v-node)
                            (cdr sucessores)
                            alfa
                            b
                            (- depth 1)
                            maximazing-player
                            player
                        )
                    )
                )
            )
        )
    )
)

(defun player-numbers-handler (max-player-number)
"Para poder alterar o tipo de linha esta função recebe se o max é o
player 1 ou 2 e retorna o inverso."
    (cond
        (
            (= max-player-number 1)
            2
        )
        (T
            1
        )
    )
)

(defun alphabeta (node depth alpha beta maximazing-player player)
    (cond
        (
            (<= depth 0)
            (best-node-info (eval-state node player) node)
            ;(eval-state node)
        )
        (T
            (cond
                (
                    maximazing-player
                    (let
                        (
                            (v (best-node-info most-negative-fixnum NIL))
                            (successors (successor-sort-dec (funcall 'get-successors node player) player))
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
                                    player
                                )
                            )
                            (T
                                ;(eval-state node)
                                (best-node-info (eval-state node player) node)
                            )
                        )
                    )
                )
                (T
                    (let
                        (
                            (v (best-node-info most-positive-fixnum NIL))
                            (successors (successor-sort (funcall 'get-successors node (player-numbers-handler player)) (player-numbers-handler player)))
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
                                    player
                                )
                            )
                            (T
                                ;(eval-state node)
                                (best-node-info (eval-state node player) node)
                            )
                        )
                    )
                )
            )
        )
    )
)
