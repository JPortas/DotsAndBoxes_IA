;;;; tests.lisp
;;;; Para realizar testes ás funções defenidas e comparar com os valores esperados.
;;;; Autor: João Portas

(defpackage :tests
    (:use
        #:cl
        #:node
        #:example-values
    )
    (:export
        #:run-tests
    )
)

(in-package :tests)

(defun run-tests ()
    (format T "Testeing... ~%")
    (format T "Test 1: ~d~%" (funcall 'test-replace-arc-connection))  
    (format T "Test 2: ~d~%" (funcall 'test-replace-arc-connection-in-position))
    (format T "Test 3: ~d~%" (funcall 'test-replace-arc-connection))  
)

(defun test-replace-arc-connection ()
    (equal (replace-arc-connection (get-connections-row-in-orientation-list (get-horizontal-arcs (ex-board)) 1) 1 1) '(1 0 0))
)

(defun test-replace-arc-connection-in-position ()
    (cond
        (
            (equal (replace-arc-connection-in-position (get-horizontal-arcs (ex-board)) 1 1) '((1 0 0) (0 0 1) (0 1 1) (0 0 1)))
            T
        )
        (T
            (format T "~T~TFailed test -> node:replace-arc-connection-in-position~%")
            NIL
        )
    )
    ;(replace-arc-connection-in-position (get-horizontal-arcs (ex-board)) 1 1)
)