(defpackage :node
    (:use
        #:COMMON-LISP
        #:board
    )
    (:export
        #:new-successor
        #:get-node-state
        #:get-node-depth
        #:get-node-parent
        #:objective-node
        #:sucessors-to-horizontal
        #:sucessors-to-vertical
        #:get-successors
        #:count-zeros
    )
)

(in-package :node)

(defun new-successor (board depth parent)
"Cria um novo sucessor em que receber uma tabuleiro, a profundidade e o nó pai e gera um nó que é uma lista ((board) depth (parent))"
    (list board depth parent)
)

(defun get-node-state (node)
"Obtém o estado de um nó que seria o tabuleiro. Ao seja de um nó ((board) 0 (parent)) ele retorna o (board)
Se for Null retorna NIL"
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

(defun get-node-depth (node)
"Obtém a profundidade de um nó que seria o tabuleiro. Ao seja de um nó ((board) 0 (parent)) ele retorna o 0.
Se for Null retorna NIL"
    (cond
        (
            (null node)
            NIL
        )
        (T
            (cadr node)
        )
    )
)

(defun get-node-parent (node)
"Obtém o pai de um nó que seria o tabuleiro. Ao seja de um nó ((board) 0 (parent)) ele retorna o (parent).
Se for Null retorna NIL."
    (cond
        (
            (null node)
            NIL
        )
        (T
            (last node)
        )
    )
)

(defun get-successors (node &optional fn-heuristic &rest rest)
"Obtem e retorna todos os sucessores do no recebidos."
;Heuristica em desenvolvimento
    (cond
        (
            (null fn-heuristic)
            (append (funcall 'sucessors-to-horizontal node) (funcall 'sucessors-to-vertical node))
        )
        (T
            ;(format t "using heuristic: ~d ~%" rest)
            ;(apply 'eval-heuristic fn-heuristic rest)
            (append
                (apply 'sucessors-to-horizontal-with-heuristic
                    node
                    (list-length (get-horizontal-arcs (get-node-state node)))
                    (list-length (get-vertical-arcs (get-node-state node)))
                    1
                    1
                    fn-heuristic
                    rest
                )
                (apply 'sucessors-to-vertical-with-heuristic
                    node
                    (list-length (get-horizontal-arcs (get-node-state node)))
                    (list-length (get-vertical-arcs (get-node-state node)))
                    1
                    1
                    fn-heuristic
                    rest
                )
            )
        )
    ) 
)

(defun sucessors-to-horizontal (node &optional (horizontal-length (list-length (get-horizontal-arcs (get-node-state node)))) (vertical-length (list-length (get-vertical-arcs (get-node-state node)))) (current-horizontal-length 1) (current-vertical-length 1))
"Gera os sucessores da colocação horizontal de uma jogada em qualquer nó que ainda não esteja preenchido. A função retorna uma lista com os sucessores."    
    (cond
        (
            (> current-horizontal-length horizontal-length)
            NIL
        )
        (
            (< current-vertical-length vertical-length)
            (cond
                (
                    (null
                        (draw-horizontal-arc
                            (get-node-state node)
                            current-horizontal-length
                            current-vertical-length
                        )
                    )
                    (sucessors-to-horizontal node horizontal-length vertical-length current-horizontal-length (+ current-vertical-length 1))
                )
                (T
                    (cons
                        (new-successor
                            (draw-horizontal-arc
                                (get-node-state node)
                                current-horizontal-length
                                current-vertical-length
                            )
                            (+ (get-node-depth node) 1)
                            node
                        )
                        (sucessors-to-horizontal node horizontal-length vertical-length current-horizontal-length (+ current-vertical-length 1))
                    )
                )
            )
        )
        (T
            (sucessors-to-horizontal node horizontal-length vertical-length (+ current-horizontal-length 1) 1)
        )
    )
)

(defun sucessors-to-vertical (node &optional (horizontal-length (list-length (get-horizontal-arcs (get-node-state node)))) (vertical-length (list-length (get-vertical-arcs (get-node-state node)))) (current-horizontal-length 1) (current-vertical-length 1))
"Gera os sucessores da colocação vertical de uma jogada em qualquer nó que ainda não esteja preenchido. A função retorna uma lista com os sucessores."   
    (cond
        (
            (> current-vertical-length vertical-length)
            NIL
        )
        (
            (< current-horizontal-length horizontal-length)
            (cond
                (
                    (null
                        (draw-vertical-arc
                            (get-node-state node)
                            current-horizontal-length
                            current-vertical-length
                        )
                    )
                    (sucessors-to-vertical node horizontal-length vertical-length (+ current-horizontal-length 1) current-vertical-length)
                )
                (T
                    (cons
                        (new-successor
                            (draw-vertical-arc
                                (get-node-state node)
                                current-horizontal-length
                                current-vertical-length
                            )
                            (+ (get-node-depth node) 1)
                            node
                        )
                        (sucessors-to-vertical node horizontal-length vertical-length (+ current-horizontal-length 1) current-vertical-length)
                    )
                )
            )
        )
        (T
            (sucessors-to-vertical node horizontal-length vertical-length 1 (+ current-vertical-length 1))
        )
    )
)

(defun count-zeros (lst)
    (cond
        (
            (null lst)
            0
        )
        (
            (numberp (car lst))
            (if (zerop (car lst))
                (+ 1 (count-zeros (cdr lst)))
                (count-zeros (cdr lst)))) 
        (T
            (+ 
                (count-zeros (car lst)) 
                (count-zeros (cdr lst))
            )
        )
    )
)