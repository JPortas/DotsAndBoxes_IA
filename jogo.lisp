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
        #:first-to-play
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

(defun game-board-empty()
    #|'(
		((1 1 1 1 1 1) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (2 2 2 2 2 2))
		((1 1 1 1 1) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (2 2 2 2 2))
	)|#
    '(
         (;arcos horizontais
            (0 0 0 0 0 0)
            (0 0 0 0 0 0)
            (0 0 0 0 0 0)
            (0 0 0 0 0 0)
            (0 0 0 0 0 0)
            (0 0 0 0 0 0)
        )
        (;arcos verticais
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
        )

    ) 
)

(defun game-board-test-3x3()
    '(
        (;arcos horizontais
            (0 0 0)
            (0 0 0)
            (0 0 0)
            (0 0 0)
        )
        (;arcos verticais
            (0 0 0)
            (0 0 0)
            (0 0 0)
            (0 0 0)
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
                (write-line "Forfeit!")
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
                                    (+ (- (funcall 'get-number-of-closed-boxes new-board) (funcall 'get-number-of-closed-boxes current-board)) (funcall 'get-p1-closed-boxes node))
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
                                    (write-line "We rejected your play. Please try again!")
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

(defun game-ia (state-node p1-turn &optional (end-game NIL))
    (cond
        (
            (not end-game)
            (format T "Score AI1: ~d~%Score AI2: ~d" (funcall 'get-p1-closed-boxes state-node) (funcall 'get-p2-closed-boxes state-node))
            (cond
                (
                    p1-turn
                    (terpri)
                    (write-line "Its AI [1] turn, please wait a little bit...")
                    (let*
                        (
                            (node-board (funcall 'alphabeta-algorithm state-node T 1))
                            (board (funcall 'get-node-state (cadr node-board)))
                            (end-game (funcall 'full-boardp board))
                        )
                        (format T "~d~%" (funcall 'game-board-to-text board))
                        (cond
                            (
                                (> (funcall 'get-p1-closed-boxes (cadr node-board)) (funcall 'get-p1-closed-boxes state-node))
                                (write-line "AI 1 closed a box! Wait for her to play again...")
                                (game-ia (cadr node-board) T end-game)
                            )
                            (T
                                (game-ia (cadr node-board) NIL end-game)
                            )
                        )
                        ;(game (cadr node-board) T)
                    )
                )
                (T
                    (terpri)
                    (write-line "Its AI [2] turn, please wait a little bit...")
                    (let*
                        (
                            (node-board (funcall 'alphabeta-algorithm state-node T 2))
                            (board (funcall 'get-node-state (cadr node-board)))
                            (end-game (funcall 'full-boardp board))
                        )
                        (format T "~d~%" (funcall 'game-board-to-text board))
                        (cond
                            (
                                (> (funcall 'get-p2-closed-boxes (cadr node-board)) (funcall 'get-p2-closed-boxes state-node))
                                (write-line "AI 2 closed a box! Wait for her to play again...")
                                (game-ia (cadr node-board) NIL end-game)
                            )
                            (T
                                (game-ia (cadr node-board) T end-game)
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
                    (write-line "AI1 Wins!")
                    (format T "Final Score AI1: ~d~%Final Score AI2: ~d" (funcall 'get-p1-closed-boxes state-node) (funcall 'get-p2-closed-boxes state-node))
                )
                (
                    (> (funcall 'get-p2-closed-boxes state-node) (funcall 'get-p1-closed-boxes state-node))
                    (write-line "Player AI2 Wins!")
                    (format T "Final Score AI1: ~d~%Final Score AI2: ~d" (funcall 'get-p1-closed-boxes state-node) (funcall 'get-p2-closed-boxes state-node))
                )
                (T
                    (write-line "Its a draw!")
                )
            )
        )
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
                    (write-line "Your turn (i.e. h21 or v11):")
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
                    (write-line "Its AI turn, please wait a little bit...")
                    (let*
                        (
                            (node-board (funcall 'alphabeta-algorithm state-node T 2))
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

(defun first-to-play ()
"Pede ao utilizador qual será o primeiro jogador a jogar."
    (write-line "Wich one should start? Enter (1) for you and (2) for IA:")
    (let
        (
            (input-choise (read-line *standard-input*))
        )
        (cond
            (
                (string-equal input-choise "1")
                T
            )
            (
                (string-equal input-choise "2")
                NIL
            )
            (T
                (write-line "Invalid option! Try again.")
                (first-to-play)
            )
        )
    )
)

(defun options-menu()
    (write-line "1. Human vs AI")
    (write-line "2. AI vs AI")
    (write-line "3. Quit")
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
                (let*
                    (
                        (p1-turn (first-to-play))
                    )
                    (let*
                        (
                            (game-board (new-successor (game-board-empty) 0 0))
                        )
                        (format T "~d~%" (funcall 'game-board-to-text (car game-board)))
                        (game game-board p1-turn)
                    )
                )
            )
            (
                (string-equal selected-option "2")
                (write-line "******************* -[ AI vs AI Mode ]- *******************")
                (let*
                    (
                        (game-board (new-successor (game-board-empty) 0 0))
                    )
                    (format T "~d~%" (funcall 'game-board-to-text (car game-board)))
                    (game-ia game-board NIL)
                )
            )
            (
                (string-equal selected-option "3")
                (write-line "Bye bye")
            )
            (t
                (write-line "Invalid entered option! Try again.")
                (options-menu)
            )
        )
    )
)


(defun start()
    (dribble "log.dat")
    (options-menu)
    (dribble)
)