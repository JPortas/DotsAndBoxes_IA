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
    )
)

(in-package :jogo)

(defun jogar()
    (funcall 'game-board-to-text (ex-board-2x6))
)