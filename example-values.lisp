;;;; example-values.lisp
;;;; Funções que retornam valores para testes no projeto.
;;;; Autor: Lucas Freixieiro e João Portas

(defpackage :example-values
    (:use
        #:COMMON-LISP
        #:node
    )
    (:export
        #:ex-board
        #:ex-board-2x6
    )
)

(in-package :example-values)


;
;   Tabuleiros
;
;;; (tabuleiro-teste)
(defun ex-board ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

(defun ex-board-2x6 ()
  "Retorna um tabuleiro 2x6"
	'(
        ((2 2 0 1 2 0) (1 0 1 0 0 0) (0 0 0 0 1 0))
		((0 2) (1 0) (1 1) (0 0) (2 1) (0 0) (0 2))
	)
)