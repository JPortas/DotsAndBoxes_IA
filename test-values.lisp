;;;; test-values.lisp
;;;; Funções que retornam valores para testes no projeto.
;;;; Autor: Lucas Freixieiro e João Portas

(defpackage :test-values
    (:use
        #:COMMON-LISP
    )
    (:export
        #:tabuleiro-teste
    )
)

(in-package :test-values)

;;; (tabuleiro-teste)
(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)