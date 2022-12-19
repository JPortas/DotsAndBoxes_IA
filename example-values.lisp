;;;; example-values.lisp
;;;; Funções que retornam valores para testes no projeto.
;;;; Autor: Lucas Freixieiro e João Portas

(defpackage :example-values
    (:use
        #:COMMON-LISP
    )
    (:export
        #:ex-board
        #:ex-node
    )
)

(in-package :example-values)

;;; (tabuleiro-teste)
(defun ex-board ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

(defun ex-node ()
    0
)