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
        #:get-node-heuristic
        #:objective-node
        #:sucessors-to-horizontal
        #:sucessors-to-vertical
        #:get-successors
        #:heuristic-eval-by-remaining-to-close
    )
)

(in-package :node)

(defun heuristic-eval-by-remaining-to-close (objective-close-squares current-closed-squares)
    (- objective-close-squares current-closed-squares)
)

(defun new-successor (board depth parent &optional heuristic)
"Cria um novo sucessor em que receber uma tabuleiro, a profundidade e o nó pai e gera um nó que é uma lista ((board) depth (parent))
Opcionalmente recebe o valor da heurística que gera um nó que é uma lista com a informação heurística ((board) depth heuristic (parent))
"
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

(defun get-node-heuristic (node)
"Obtém a heurística de um nó que seria o tabuleiro. Ao seja de um nó ((board) 0 10 (parent)) ele retorna o 10.
Se for Null retorna NIL. Se a heuristica não existir retorna NIL."
    (cond
        (
            (null node)
            NIL
        )
        (
            (listp (caddr node))
            NIL
        )
        (T
            (caddr node)
        )
    )
)

(defun get-successors (node &optional fn-heuristic)
"Obtem e retorna todos os sucessores do no recebidos."
;Heuristica em desenvolvimento
    (cond
        (
            (null fn-heuristic)
            (append (funcall 'sucessors-to-horizontal node) (funcall 'sucessors-to-vertical node))
        )
        (T
            (funcall fn-heuristic 5 3)
        )
    ) 
)

(defun objective-node (node objective-closed-boxes)
"Compara a quantidade de caixas fechadas no estado de um nó com as objetivo.
Se for igual ou maior retorna T. Se for menor retorna NIL
(ATUALMENTE NÃO FUNCIONANDO COMO DIZ ACIMA E SIM COMPARANDO COM UMA SOLUÇÃO ESTÁTICA)"
    (cond
        (
            (>= (get-number-of-closed-boxes (get-node-state node)) objective-closed-boxes)
            T
        )
        (T
            NIL
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
            ;(format t "~%~d ~d~%" current-horizontal-length current-vertical-length)
            ;(sucessors-to-horizontal node horizontal-length vertical-length current-horizontal-length (+ current-vertical-length 1))
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
            ;(format t "~%~d ~d~%" current-horizontal-length current-vertical-length)
            ;(format t "~%~d < ~d~%" current-horizontal-length horizontal-length)
            ;(sucessors-to-vertical node horizontal-length vertical-length (+ current-horizontal-length 1) current-vertical-length)
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