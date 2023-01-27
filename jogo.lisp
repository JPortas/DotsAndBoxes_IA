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
    #|(let*
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
    )|#
    
    ;(funcall 'alphabeta-algorithm (new-successor (game-board-test-empty) 0 0))
)

(defun string-to-list (str &optional (index 0))
"Converte uma string em uma lista. Ex: Input `h12` Output: ('h' '1' '2')"
    (cond
        (
            (/= index (length str))
            (cons (subseq str index (+ index 1)) (string-to-list str (+ index 1)))
        )
        (
            NIL
        )
    )
)

(defun get-player-input ()
"Pede o input de uma jogada a um utilizador e retorna uma list com 3 posições
em que a primeira é a direção a segunda a posição 1 e a treçeira posição 2.
Output: ('h' '1' '2')
inserir `e` para sair"
    (let
        (
            (input-play (read-line *standard-input*))
        )
        (cond
            (
                (string-equal input-play "e")
                (write-line "Surrendered!")
                "exit"
            )
            (
                (= (length input-play) 3)
                (string-to-list input-play)
            )
            (T
                (write-line "Incorrect input format! Try again.")
                (get-player-input)
            )
        )
    )
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
                (cond ;Jogada válida
                    (
                        new-board
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
                    (T ;Jogada inválida
                        NIL
                    )
                )
            )
            (T
            0
            )
        )
    )
)

(defun player-turn (state-node)
    (let*
                (
                    (player-input (funcall 'get-player-input))
                )
                (cond
                    (
                        (listp player-input)
                        (let*
                            (
                                (next-move-node (do-play-player state-node (car player-input) (parse-integer (cadr player-input)) (parse-integer (caddr player-input)) 1))
                            )
                            (cond
                                (
                                    (not (null next-move-node))
                                    next-move-node
                                )
                                (T
                                    (write-line "We rejected your play. Ples try again but not the same!")
                                    (player-turn state-node)
                                )
                            )
                        )
                    )
                    (T
                        (write-line "Backing to the main menu...")
                    )
                )
                #|(format NIL "~d"
                    (funcall 'game-board-to-text
                        (funcall 'get-node-state state)
                    )
                )|#
            )
)

(defun game(state-node p1-turn &optional (end-game NIL))
    (cond
        (
            (not end-game)
            (format T "Score P1: ~d~%Score P2: ~d" (funcall 'get-p1-closed-boxes state-node) (funcall 'get-p2-closed-boxes state-node))
            (cond
                (
                    p1-turn
                    (terpri)
                    (write-line "Your play (h21 or v11):")
                    (let*
                        (
                            (node-board (player-turn state-node))
                            (end-game (funcall 'full-boardp (funcall 'get-node-state node-board)))
                        )
                        (format T "~d~%" (funcall 'game-board-to-text (car node-board)))
                        (cond
                            (
                                (> (funcall 'get-p1-closed-boxes node-board) (funcall 'get-p1-closed-boxes state-node))
                                (write-line "You closed a box! Do another play.")
                                (game node-board T end-game)
                            )
                            (T
                                (game node-board NIL end-game)
                            )
                        )
                    )
                )
                (T
                    (terpri)
                    (write-line "Its the playing, pleas wait some time...")
                    (let*
                        (
                            (node-board (funcall 'alphabeta-algorithm state-node))
                            (board (funcall 'get-node-state (cadr node-board)))
                            (end-game (funcall 'full-boardp board))
                        )
                        (format T "~d~%" (funcall 'game-board-to-text board))
                        (cond
                            (
                                (> (funcall 'get-p2-closed-boxes (cadr node-board)) (funcall 'get-p2-closed-boxes state-node))
                                (write-line "AI closed a box! Wait for her to play again...")
                                (game (cadr node-board) NIL end-game)
                            )
                            (T
                                (game (cadr node-board) T end-game)
                            )
                        )
                        ;(game (cadr node-board) T)
                    )                    
                )
            )
        )
        (T ;game end
            (cond
                (
                    (> (funcall 'get-p1-closed-boxes state-node) (funcall 'get-p2-closed-boxes state-node))
                    (write-line "Player 1 Wins!")
                    (format T "Final Score P1: ~d~%Final Score P2: ~d" (funcall 'get-p1-closed-boxes state-node) (funcall 'get-p2-closed-boxes state-node))
                )
                (
                    (> (funcall 'get-p2-closed-boxes state-node) (funcall 'get-p1-closed-boxes state-node))
                    (write-line "Player 2 Wins!")
                    (format T "Final Score P1: ~d~%Final Score P2: ~d" (funcall 'get-p1-closed-boxes state-node) (funcall 'get-p2-closed-boxes state-node))
                )
                (T
                    (write-line "Its a draw!")
                )
            )
        )
    )
    

    
)

(defun options-menu()
    (write-line "1. Human vs AI")
    (write-line "2. AI vs AI")
    (write-line "3. Quit from the school")
    (write-line "4. Escape to a paralel world")
    (write-line "5. Quit")
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
                (format T "~d~%" (funcall 'game-board-to-text (car (new-successor (game-board-test-empty) 0 0))))
                (game (new-successor (game-board-test-empty) 0 0) T)
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