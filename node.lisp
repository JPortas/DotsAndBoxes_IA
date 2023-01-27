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
        #:get-p1-closed-boxes
        #:get-p2-closed-boxes
        #:draw-arc
    )
)

(in-package :node)

(defun new-successor (board p1 p2)
"Cria um novo sucessor em que receber uma tabuleiro, a profundidade, quantas caixas tem o jogador um fechado e quantas o 2 tem fechadas"
    (list board p1 p2)
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

(defun draw-arc (node orientation pos1 pos2 player)
    (cond
        (
            (string-equal orientation "horizontal")
            (draw-horizontal-arc
                (get-node-state node)
                pos1
                pos2
                player
            )
        )
        (T
            (draw-vertical-arc
                (get-node-state node)
                pos1
                pos2
                player
            )
        )
    )
)

(defun get-p1-closed-boxes (node)
"Obtem a quantidade de caixas fechadas pelo player 1 do nó."
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

(defun get-p2-closed-boxes (node)
"Obtem a quantidade de caixas fechadas pelo player 2 do nó."
    (cond
        (
            (null node)
            NIL
        )
        (T
            (caddr node)
        )
    )
)

(defun get-successors (node player)
"Obtem e retorna todos os sucessores do no recebidos."
    (append (funcall 'sucessors-to-horizontal node player) (funcall 'sucessors-to-vertical node player))
)

(defun sucessors-to-horizontal (node player &optional (horizontal-length (list-length (get-horizontal-arcs (get-node-state node)))) (vertical-length (list-length (get-vertical-arcs (get-node-state node)))) (current-horizontal-length 1) (current-vertical-length 1))
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
                            player
                        )
                    )
                    (sucessors-to-horizontal node player horizontal-length vertical-length current-horizontal-length (+ current-vertical-length 1))
                )
                (T
                    (let
                        (
                            (generated-board
                                (draw-horizontal-arc
                                    (get-node-state node)
                                    current-horizontal-length
                                    current-vertical-length
                                    player
                                )
                            )
                        )
                        (cons

                            (cond
                                (
                                    (= player 1)
                                    (new-successor
                                        generated-board
                                        (cond
                                            (
                                                (>  (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                (+ 1 (get-p1-closed-boxes node))
                                            )
                                            (T
                                                (get-p1-closed-boxes node)
                                            )
                                        )
                                        (get-p2-closed-boxes node)
                                    )
                                )
                                (T
                                    (new-successor
                                        generated-board
                                        (get-p1-closed-boxes node)
                                        (cond
                                            (
                                                (>  (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                (+ 1 (get-p2-closed-boxes node))
                                            )
                                            (T
                                                (get-p2-closed-boxes node)
                                            )
                                        )
                                    )
                                )
                            )
                            (sucessors-to-horizontal node player horizontal-length vertical-length current-horizontal-length (+ current-vertical-length 1))
                        )
                    )
                )
            )
        )
        (T
            (sucessors-to-horizontal node player horizontal-length vertical-length (+ current-horizontal-length 1) 1)
        )
    )
)

(defun sucessors-to-vertical (node player &optional (horizontal-length (list-length (get-horizontal-arcs (get-node-state node)))) (vertical-length (list-length (get-vertical-arcs (get-node-state node)))) (current-horizontal-length 1) (current-vertical-length 1))
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
                            player
                        )
                    )
                    (sucessors-to-vertical node player horizontal-length vertical-length (+ current-horizontal-length 1) current-vertical-length)
                )
                (T
                    (let
                        (
                            (generated-board
                                (draw-vertical-arc
                                    (get-node-state node)
                                    current-horizontal-length
                                    current-vertical-length
                                    player
                                )
                            )
                        )
                        (cons
                            (cond
                                (
                                    (= player 1)
                                    (new-successor
                                        generated-board
                                        (cond
                                            (
                                                (>  (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                ;(format t "~d > ~d ~%" (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                (+ 1 (get-p1-closed-boxes node))
                                            )
                                            (T
                                                ;(format t "~d > ~d ~%" (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                (get-p1-closed-boxes node)
                                            )
                                        )
                                        (get-p2-closed-boxes node)
                                    )
                                )
                                (T
                                    (new-successor
                                        generated-board
                                        (get-p1-closed-boxes node)
                                        (cond
                                            (
                                                (>  (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                ;(format t "~d > ~d ~%" (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                (+ 1 (get-p2-closed-boxes node))
                                            )
                                            (T
                                                ;(format t "~d > ~d ~%" (get-number-of-closed-boxes generated-board) (get-number-of-closed-boxes (get-node-state node)))
                                                (get-p2-closed-boxes node)
                                            )
                                        )
                                    )
                                )
                            )
                            (sucessors-to-vertical node player horizontal-length vertical-length (+ current-horizontal-length 1) current-vertical-length)
                        )
                    )
                )
            )
        )
        (T
            (sucessors-to-vertical node player horizontal-length vertical-length 1 (+ current-vertical-length 1))
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