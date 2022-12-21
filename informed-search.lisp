(defpackage :informed-search
    (:use
        #:COMMON-LISP
        #:node
    )
    (:export
        #:a-star-init
        #:a-star
        #:sort-by-heuristic-value
        #:add-to-open-list-a-star
        #:get-node-if-exists
        #:create-open-list-nodes
        #:remove-from-closed-list
    )
)

(in-package :informed-search)

(defun a-star-init (start-node closed-boxes-objective fn-heuristic &rest rest)
"Recebe o no de inicio, quantidade de caixas fechadas no objetivo e a função heuristica.
A fn-heuristic seria '[heuristiva] [parameters] que recebe o simbolo da função euristica e os parametros.
Têm que estar defenidos no package node por enquanto."
    (apply 'a-star fn-heuristic closed-boxes-objective (list start-node) NIL rest)
)

(defun a-star (fn-heuristic closed-boxes-objective OPEN-LIST &optional CLOSED-LIST &rest rest)
    (cond
        (
            (null OPEN-LIST)
            NIL
        )
        (T
            (let
                (
                    (successors (apply 'get-successors (car OPEN-LIST) fn-heuristic rest))
                )
                (let
                    (
                        (objective-node (funcall 'get-objective-node closed-boxes-objective successors))
                    )
                    (cond
                        (
                            (null objective-node)
                            (let
                                (
                                    (ordered-list (funcall 'sort-by-heuristic-value (funcall 'create-open-list-nodes successors OPEN-LIST CLOSED-LIST)))
                                )
                                ;ordered-list
                                (apply 'a-star
                                    fn-heuristic
                                    closed-boxes-objective
                                    ordered-list
                                    (append (list (car OPEN-LIST)) (funcall 'remove-from-closed-list ordered-list CLOSED-LIST))
                                    rest
                                )
                            )
                            #|(a-star
                                fn-heuristic
                                closed-boxes-objective
                                (funcall 'create-open-list-nodes (cdr OPEN-LIST) successors)
                                (append (list (car OPEN-LIST)) CLOSED-LIST)
                            )|#
                            
                            ;successors
                            ;(funcall 'add-to-open-list-a-star (cdr OPEN-LIST) successors)
                        )
                        (T
                            objective-node
                        )
                    )
                )
            )
        )
    )
)

(defun remove-from-closed-list (successors-list closed-list)
    (cond
        (
            (null closed-list)
            NIL
        )
        (T
            (cond
                (
                    (null (funcall 'get-node-if-exists (car closed-list) successors-list))
                    (cons (car closed-list) (remove-from-closed-list successors-list (cdr closed-list)))
                )
                (T
                    (remove-from-closed-list successors-list (cdr closed-list))
                )
            )
        )
    )
)

(defun sort-by-heuristic-value (nodes-list)
"Ordena ascendentemente uma lista (pode ser a de abertos) baseado no valor heurístico de cada nó"
    (sort
        nodes-list
        (lambda (x y)
            (<
                (funcall 'get-node-heuristic x)
                (funcall 'get-node-heuristic y)
            )
        )
    )
)

(defun add-to-open-list-a-star (OPEN-LIST successors-list)
"Função obseleta? (create-open-list-nodes) substitui esta apenas sendo necessário ordenar."
    (cond
        (
            (and (null OPEN-LIST) (null successors-list))
            NIL
        )
        (
            (null OPEN-LIST)
            (sort-by-heuristic-value successors-list)
        )
        (
            (null successors-list)
            OPEN-LIST
        )
        (T
            (sort-by-heuristic-value (append OPEN-LIST successors-list))
        )
    )
)

(defun get-objective-node (closed-boxes-objective successors)
"Recebe quantas caixas são fechadas para objetivo e os sucessores de um nó que são verificados se é o objetivo final (solução)
Se sim retorna o nó. Se não for retorna NIL"
    (cond
        (
            (null successors)
            NIL
        )
        (
            (funcall 'objective-node (car successors) closed-boxes-objective)
            (car successors)
        )
        (
            (get-objective-node closed-boxes-objective (cdr successors))
        )
    )
)

;(filter-successors-by-news-and-low-cost (generated-successors-heuristic) (existing-closed-heuristic) NIL)
(defun create-open-list-nodes (successors-list OPEN-LIST CLOSED-LIST)
"Cria uma lista desordenada já com os abertos todos. Recebe os sucessores os abertos atuais e os fechados.
Retorna uma lista desordenada com os sucessores novos + entre os sucessores com estados iguais aos abertos ou fechados os em que a
heuristica seja menor (nó mais proximo da solução)."
    (cond
        (
            (null successors-list)
            NIL
        )
        (
            (not (null (funcall 'get-node-if-exists (car successors-list) CLOSED-LIST)))
                (cond
                    (
                        (< (funcall 'get-node-heuristic (car successors-list)) (funcall 'get-node-heuristic (funcall 'get-node-if-exists (car successors-list) CLOSED-LIST)))
                        (cons (car successors-list) (create-open-list-nodes (cdr successors-list) OPEN-LIST CLOSED-LIST))
                    )
                    (T
                        (cons (funcall 'get-node-if-exists (car successors-list) CLOSED-LIST) (create-open-list-nodes (cdr successors-list) OPEN-LIST CLOSED-LIST))
                    )
                )
        )
        (
            (not (null (funcall 'get-node-if-exists (car successors-list) OPEN-LIST)))
                (cond
                    (
                        (< (funcall 'get-node-heuristic (car successors-list)) (funcall 'get-node-heuristic (funcall 'get-node-if-exists (car successors-list) OPEN-LIST)))
                        (cons (car successors-list) (create-open-list-nodes (cdr successors-list) OPEN-LIST CLOSED-LIST))
                    )
                    (T
                        (cons (funcall 'get-node-if-exists (car successors-list) OPEN-LIST) (create-open-list-nodes (cdr successors-list) OPEN-LIST CLOSED-LIST))
                    )
                )
        )
        (T
            (cons (car successors-list) (create-open-list-nodes (cdr successors-list) OPEN-LIST CLOSED-LIST))
        )
    )
)

(defun get-node-if-exists (node compare-list)
"Se um nó existe na lista recebida retorna o nó senão retorna NIL. O estado é o que é comparado então a profundidade ou heurisca
não fazem diferença."
    ;(format t "State1: ~% ~d ~%" (funcall 'get-node-state node))
    ;(format t "State2: ~% ~d ~%" (funcall 'get-node-state (car compare-list)))
    ;(format t "Comp: ~% ~d ~%" (equal (funcall 'get-node-state node) (funcall 'get-node-state (car compare-list))))
    (cond
        (
            (null compare-list)
            NIL
        )
        (
            (equal (funcall 'get-node-state node) (funcall 'get-node-state (car compare-list)))
            (car compare-list)
        )
        (
            (get-node-if-exists node (cdr compare-list))
        )
    )
)