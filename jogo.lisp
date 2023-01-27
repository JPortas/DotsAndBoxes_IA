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
            (1 1)
            (1 0)
            (2 0)
        )
        (;arcos verticais
            (1 2)
            (1 2)
            (1 0)
        )
    ) 
)

(defun game-board-test-empty()
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
    ;(funcall 'alphabeta-algorithm (new-successor (game-board) 8 4))
    (let*
        (
            (jg0 (funcall 'get-node-state (new-successor (game-board) 8 4)))
            (jg1 (cadr (funcall 'alphabeta-algorithm (new-successor (game-board) 8 4))))
            (jg2 (funcall 'alphabeta-algorithm jg1))
        )
        (princ (funcall 'game-board-to-text jg0))
         (princ
                (funcall 'game-board-to-text
                    (funcall 'get-node-state jg1)
                )
            )
        (princ
                (funcall 'game-board-to-text
                    (funcall 'get-node-state (cadr jg2))
                )
            )
    )
    
    ;(funcall 'alphabeta-algorithm (new-successor (game-board-test-empty) 0 0))
)

(defun do-play-player (node orientation line1 line2 player)
    (let*
        (
            (current-board (funcall 'get-node-state node))
            (new-board (funcall 'draw-arc node orientation line1 line2 player))
        )
        (cond
            (
                (= player 1)
                (funcall 'new-successor
                    new-board
                    (cond
                        (
                            (> (funcall 'get-number-of-closed-boxes new-board) (funcall 'get-number-of-closed-boxes current-board))
                            (+ 1 (funcall 'get-p1-closed-boxes node))
                        )
                        (T
                            (funcall 'get-p1-closed-boxes node)
                        )
                    )
                    (funcall 'get-p2-closed-boxes node)
                )
            )
            (T
            0
            )
        )
    )
)

(defun game(state-node p1-turn &optional end-game)
    (let
        (
            (state state-node)
        )
        (format NIL "~d"
            (funcall 'game-board-to-text
                (funcall 'get-node-state state)
            )
        )
        (do-play-player state-node "vertical" 1 2 1)
        #|(format NIL "~d"
            (funcall 'game-board-to-text
                (funcall 'get-node-state state)
            )
        )|#
    )
)

(defun options-menu()
    (write-line "1. Human vs AI")
    (write-line "2.AI vs AI")
    (write-line "3.Quit from the school")
    (write-line "4.Escape to a paralel world")
    (write-line "5.Quit")
    (terpri)
    (write-line "Digit an options and press ENTER:")
    (let 
        (
            (selected-option (read-line *standard-input*))
        )
        (cond
            (
                (string-equal selected-option "1")
                (write-line "******************* -[ Human vs AI Mode ]- *******************")
                (game (new-successor (game-board-test-empty) 0 0) T NIL)
            )
            (
                (string-equal selected-option "2")
                (write-line "SINGULARITY?")
            )
            (
                (string-equal selected-option "3")
                (write-line "We are gone")
            )
            (t
                (write-line "Invalid entered option! Try again.")
                (options-menu)
            )
        )
    )
)


(defun start()
    (options-menu)
)