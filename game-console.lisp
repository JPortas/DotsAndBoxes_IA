;;;; game-console.lisp
;;;; Responsável por criar, produzir registar dados relaticamente á situação do jogo.
;;;; Autor: Lucas Freixieiro e João Portas

(defpackage :game-console
    (:use
        #:COMMON-LISP
        #:board
    )
    (:export
        #:game-board-to-text
        #:create-game-board-text
        #:add-board-line-to-text
        #:repeat-symbol-string

        #:list-with-each-n
        #:vertical-to-horizontal-arcs-list
    )
)

(in-package :game-console)

;;
;; Funções para obter estado do jogo para apresentar na consola
;;
(defun board-lines-list-to-text-horizontal (board-lines-list point-symbol space-between-symbol-h p1-symbol p2-symbol &optional (line-text ""))
"Recebe uma lista das linhas horizontai, e os símbolos para o ponto de conecção o espaço e as marcas de cada jogador e retorna uma linha
que corresponde ás conecções em representação gráfica."
    (cond
        (
            (null board-lines-list)
            (concatenate 'string line-text point-symbol)
        )
        (
            (= (car board-lines-list) 1)
            (board-lines-list-to-text-horizontal (cdr board-lines-list) point-symbol space-between-symbol-h p1-symbol p2-symbol (concatenate 'string line-text point-symbol p1-symbol))
        )
        (
            (= (car board-lines-list) 2)
            (board-lines-list-to-text-horizontal (cdr board-lines-list) point-symbol space-between-symbol-h p1-symbol p2-symbol (concatenate 'string line-text point-symbol p2-symbol))
        )
        (T
            (board-lines-list-to-text-horizontal (cdr board-lines-list) point-symbol space-between-symbol-h p1-symbol p2-symbol (concatenate 'string line-text point-symbol space-between-symbol-h))
        )
    )
)

(defun board-lines-list-to-text-vertical (board-lines-list point-symbol space-between-symbol-h p1-symbol p2-symbol &optional (line-text ""))
"Recebe uma lista das linhas verticais, e os símbolos para o ponto de conecção o espaço e as marcas de cada jogador e retorna uma linha
que corresponde ás conecções em representação gráfica."
    (cond
        (
            (null board-lines-list)
            (concatenate 'string line-text " ")
        )
        (
            (= (car board-lines-list) 1)
            (board-lines-list-to-text-vertical (cdr board-lines-list) point-symbol space-between-symbol-h p1-symbol p2-symbol (concatenate 'string line-text p1-symbol space-between-symbol-h))
        )
        (
            (= (car board-lines-list) 2)
            (board-lines-list-to-text-vertical (cdr board-lines-list) point-symbol space-between-symbol-h p1-symbol p2-symbol (concatenate 'string line-text p2-symbol space-between-symbol-h))
        )
        (T
            (board-lines-list-to-text-vertical (cdr board-lines-list) point-symbol space-between-symbol-h p1-symbol p2-symbol (concatenate 'string line-text " " space-between-symbol-h))
        )
    )
)

(defun list-with-each-n (vertical-arcs position)
"Recebe a lista de arcos verticais e uma posição e retorna uma outra de arcos horizontai só
que em que a ordem de cada sublista coresponde á colocação dos arcos verticais na mesma orientação que os horizontais."
    (cond
        (
            (null vertical-arcs)
            NIL
        )
        (T
            (cons (nth position (car vertical-arcs)) (list-with-each-n (cdr vertical-arcs) position))
        )
    )
)

(defun vertical-to-horizontal-arcs-list (horizontal-arcs-length vertical-arcs &optional (horizontal-counter 0))
    (cond
        (
            (< horizontal-counter (- horizontal-arcs-length 1))
            (cons (list-with-each-n vertical-arcs horizontal-counter) (vertical-to-horizontal-arcs-list horizontal-arcs-length vertical-arcs (+ horizontal-counter 1)))
        )
        (T
            NIL
        )
    )
)

(defun create-game-board-text (horizontal-arcs vertical-arcs point-symbol space-between-symbol-h p1-symbol-h p1-symbol-v p2-symbol-h p2-symbol-v &optional (board-print-text "") (orientation "horizontal"))
"Cria, preenche e retorna um estado do tabuleiro completo com os símbolos recebidos e dados."    
    (cond
        (
            (or (not (null horizontal-arcs)) (not (null horizontal-arcs)))
            (let
                (
                    (h-line 
                        (cond
                            (
                                (string-equal orientation "horizontal")
                                (board-lines-list-to-text-horizontal (car horizontal-arcs) point-symbol space-between-symbol-h p1-symbol-h p2-symbol-h)
                            )
                            (T
                                (board-lines-list-to-text-vertical (car vertical-arcs) point-symbol space-between-symbol-h p1-symbol-v p2-symbol-v)
                            )
                        )
                    )
                    ;(v-line (board-lines-list-to-text-vertical (car vertical-arcs) point-symbol space-between-symbol-h p1-symbol-v p2-symbol-v))
                    ;(h-line2 (board-lines-list-to-text-horizontal (car horizontal-arcs) point-symbol space-between-symbol-h p1-symbol-h p2-symbol-h))
                )
                ;(concatenate 'string h-line "\n" v-line)
                (let
                    (
                        ;(result (format NIL "~a~%~a~%~a" h-line v-line h-line2))
                        (result (format NIL "~a~%~a" board-print-text h-line))
                    )
                    ;result
                    (cond
                        (
                            (string-equal orientation "horizontal")
                            (create-game-board-text (cdr horizontal-arcs) vertical-arcs point-symbol space-between-symbol-h p1-symbol-h p1-symbol-v p2-symbol-h p2-symbol-v result "vertical")
                        )
                        (T
                            (create-game-board-text horizontal-arcs (cdr vertical-arcs) point-symbol space-between-symbol-h p1-symbol-h p1-symbol-v p2-symbol-h p2-symbol-v result "horizontal")
                        )
                    )
                )
            )
        )
        (T
            board-print-text
        )
    )
)

(defun repeat-symbol-string (character-symbol times)
"Recebe um caractere e retorna uma string em que o caracteres é repetido o número de vezes recebido."
    (cond
        (
            (> times 0)
            (concatenate 'string character-symbol (repeat-symbol-string character-symbol (- times 1)))
        )
        (T
            NIL
        )
    )
)

(defun game-board-to-text (board &optional (point-symbol "■") (space-between-symbol-h " ") (p1-symbol-h "─") (p1-symbol-v "│") (p2-symbol-h "-") (p2-symbol-v "¦"))
"Recebe um tabuleiro e opcionalmente dados que iram representar o ponto de conecção, sem conecxão, e símbolos para representar conecxões h -> horizontais
e v -> verticais de cada jogador. No final retorna uma variável com a representação do tabuleiro."    
    (create-game-board-text
        (funcall 'get-horizontal-arcs board)
        (vertical-to-horizontal-arcs-list (list-length (funcall 'get-horizontal-arcs board)) (funcall 'get-vertical-arcs board))
        point-symbol
        (repeat-symbol-string space-between-symbol-h 3)
        (repeat-symbol-string p1-symbol-h 3)
        p1-symbol-v
        (repeat-symbol-string p2-symbol-h 3)
        p2-symbol-v
    )
)
