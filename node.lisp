;;;; node.lisp
;;;; Possuí funções para consulta e criação de estados e valores de estados e seletores para os nós
;;;; Autor: Lucas Freixieiro e João Portas

(defpackage :node
    (:use
        #:COMMON-LISP
    )
    (:export
        #:new-node
        #:get-horizontal-lines
        #:get-vertical-lines
    )
)

(in-package :node)

(defun new-node (state depth &optional (parent-node NIL))
    (list state depth parent-node)
)

(defun get-horizontal-lines (board)
"Recebe um tabuleiro e retorna uma lista com sublistas em que cada representa um 'linha' horizontal do tabuleiro.
Parameters >>
    board (list): Representa um tabuleiro de jogo.
        exemplo:
        (
            ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
            ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
        )
Return <<
    (list): Um lista com as infromações dos arcos horizontais.
        exemplo:
        ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
    OR
    NIL: Se os parametros recebidos forem NULL.
    

"
	(cond
		(
			(null board)
			nil
		)
		(T
			(car board)
		)
	)
)

(defun get-vertical-lines (board)
"Recebe um tabuleiro e retorna uma lista com sublistas em que cada representa um 'linha' vertical do tabuleiro.
Parameters >>
    board (list): Representa um tabuleiro de jogo.
        exemplo:
        (
            ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
            ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
        )
Return <<
    (list): Um lista com as infromações dos arcos verticais.
        exemplo:
        ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
    OR
    NIL: Se os parametros recebidos forem NULL.
    

"
	(cond
		(
			(null board)
			nil
		)
		(T
			(cadr board)
		)
	)
)

(defun hello (name)
    (format T "Hello ~d!" name)
)

(defun ups (name)
    (format T "Hello ~d!" name)
)