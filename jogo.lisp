(load "board.lisp")
(load "node.lisp")

(load "alphabeta-algorithm.lisp")

(load "example-values.lisp")

(load "game-console.lisp")

(defpackage :jogo
    (:use
        #:COMMON-LISP
        #:COMMON-LISP-USER
        #:board
        #:node
        #:alphabeta-algorithm
        #:game-console
        #:example-values
    )
    (:import-from
        #:COMMON-LISP-USER #:exit
    )
    (:export
        #:jogar
        #:game-board
    )
)

(in-package :jogo)

(defun game-board()
    #|'(
		((1 1 1 1 1 1) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (2 2 2 2 2 2))
		((1 1 1 1 1) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (2 2 2 2 2))
	)|#
    '(
        (;arcos horizontais
            (1 2 1 1 0 2)
            (2 1 1 1 1 0)
            (0 2 1 1 2 0)
            (0 1 0 2 2 0)
            (1 2 0 0 0 0)
            (0 1 2 1 2 1)
        )
        (;arcos verticais
            (1 0 1 0 0)
            (2 1 1 2 2)
            (2 1 1 2 0)
            (1 2 2 1 1)
            (1 2 2 0 0)
            (0 1 2 1 2)
            (2 2 1 2 0)
        )
    ) 
)

(defun game-board-test()
    #|'(
		((1 1 1 1 1 1) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (2 2 2 2 2 2))
		((1 1 1 1 1) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (2 2 2 2 2))
	)|#
    '(
        (;arcos horizontais
            (0 0)
            (0 0)
            (0 0)
        )
        (;arcos verticais
            (0 0)
            (0 0)
            (0 0)
        )
    ) 
)

(defun print-sucessors (sucessors)
    (cond
        (
            (not (null sucessors))
            (princ
                (funcall 'game-board-to-text
                    (funcall 'get-node-state (car sucessors))
                )
            )
            (format T "~%P1: ~d P2: ~d" (funcall 'get-p1-closed-boxes (car sucessors)) (funcall 'get-p2-closed-boxes (car sucessors)))
            (print "--------------")
            (print-sucessors (cdr sucessors))
        )
        (T
            NIL
        )
    )
)

(defun jogar()
    ;(funcall 'game-board-to-text (game-board))
    ;(funcall 'new-successor (game-board) 0 NIL)
    ;(print-sucessors
        ;(get-successors (new-successor (game-board) 0 0 1) 1)
        ;(funcall 'get-successors 
        ;    (funcall 'new-successor (game-board) 0 NIL)
        ;)
    ;)
    ;(funcall 'get-p1-closed-boxes (funcall 'new-successor (game-board) 0 1 4))
    (funcall 'alphabeta-algorithm (new-successor (game-board) 8 4))
)
