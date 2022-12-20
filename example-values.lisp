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
        #:ex-objective-board
        #:ex-node
        #:ex-objective-node
        #:ex-board-4x2
        #:ex-board-4x3
        #:ex-node-4x3
        #:ex-node-4x2
        #:ex-board-6x2
        #:ex-board-2x6
        #:ex-node-6x2
        #:ex-node-2x6
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

(defun ex-objective-board ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
        (
            (0 1 0) (0 1 1) (0 1 1) (0 1 1)
        )
        (
            (0 0 0) (1 1 1) (1 1 1) (0 1 1)
        )
    )
)

(defun ex-board-4x2 ()
  "Retorna um tabuleiro 4x2 (3 arcos na vertical por 2 arcos na horizontal)"
	'(
		((0 0) (0 0) (0 1) (0 0) (0 0))
		((0 0 0 0) (0 1 1 0) (1 0 1 0))
	)
)

(defun ex-board-6x2 ()
  "Retorna um tabuleiro 4x2 (3 arcos na vertical por 2 arcos na horizontal)"
	'(
		((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))
		((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))
	)
)

(defun ex-board-2x6 ()
  "Retorna um tabuleiro 4x2 (3 arcos na vertical por 2 arcos na horizontal)"
	'(
        ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))
		((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))
	)
)

(defun ex-board-4x3 ()
  "Retorna um tabuleiro 4x2 (3 arcos na vertical por 2 arcos na horizontal)"
	'(
		((0 0 0) (0 0 0) (0 0 0) (0 0 0))
		((0 0 0 0) (0 0 0 0) (0 0 0 0))
	)
)

(defun ex-objective-node (&optional heuristic)
"Retorna um nó com um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) preenchido com 5 caixas fechadas.
Pode incluir heuristica ou não por parametro."
	(cond
        (
            (null heuristic)
            (new-successor (ex-objective-board) 0 NIL)
        )
        (T
            (new-successor (ex-objective-board) 0 NIL 0)
        )
    )
)

(defun ex-node-4x3 (&optional heuristic)
"Retorna um nó com um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal).
Pode incluir heuristica ou não por parametro."
    (cond
        (
            (null heuristic)
            (new-successor (ex-board-4x3) 0 NIL)
        )
        (T
            (new-successor (ex-board-4x3) 0 NIL 10)
        )
    )
)


(defun ex-node-6x2 (&optional heuristic)
"Retorna um nó com um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal).
Pode incluir heuristica ou não por parametro."
    (cond
        (
            (null heuristic)
            (new-successor (ex-board-6x2) 0 NIL)
        )
        (T
            (new-successor (ex-board-6x2) 0 NIL 10)
        )
    )
)

(defun ex-node-2x6 (&optional heuristic)
"Retorna um nó com um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal).
Pode incluir heuristica ou não por parametro."
    (cond
        (
            (null heuristic)
            (new-successor (ex-board-2x6) 0 NIL)
        )
        (T
            (new-successor (ex-board-2x6) 0 NIL 10)
        )
    )
)

(defun ex-node-4x2 (&optional heuristic)
"Retorna um nó com um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal).
Pode incluir heuristica ou não por parametro."
    (cond
        (
            (null heuristic)
            (new-successor (ex-board-4x2) 0 NIL)
        )
        (T
            (new-successor (ex-board-4x2) 0 NIL 10)
        )
    )
)

(defun ex-node (&optional heuristic)
"Retorna um nó com um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal).
Pode incluir heuristica ou não por parametro."
    (cond
        (
            (null heuristic)
            (new-successor (ex-board) 0 NIL)
        )
        (T
            (new-successor (ex-board) 0 NIL 10)
        )
    )
)