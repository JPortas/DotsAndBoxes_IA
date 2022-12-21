(load "board.lisp")
(load "node.lisp")

(load "not-informed-search.lisp")
(load "informed-search.lisp")

(load "example-values.lisp")
(load "unit-tests.lisp")

(defpackage #:main
    (:use
        #:COMMON-LISP
        #:COMMON-LISP-USER
        #:board
        #:node
        #:informed-search
        #:not-informed-search
        #:example-values
        #:unit-tests
    )
    (:import-from
        #:COMMON-LISP-USER #:exit
    )
    (:export
        #:main
        #:load-problems
        #:read-lists-from-string
        #:exit
    )
)

(in-package :main)

;execute
;sbcl --load .\main.lisp --eval '(in-package #:main)' --eval '(main)'
;to start your jorney or
;sbcl --load .\main.lisp --eval '(in-package #:main)'
;to teste by your own

(defun remove-nil-from-list (lista)
    (cond
        (
            (null lista)
            nil
        )
        (
            (null (car lista))
            (remove-nil-from-list (cdr lista))
        )
        (T
            (cons (car lista) (remove-nil-from-list (cdr lista)))
        )
    )
)

(defun create-list-from-file (filename)
    (cond
        (
            (not (stringp filename))
            nil
        )
        (t  
            (with-open-file (stream filename)
                (let ((line (read-line stream)))
                    (with-input-from-string (s (string line))
                        (loop
                            for line = (read s nil)
                            while line
                            collect (cond
                                (
                                    (eql line '/)
                                    (progn nil)
                                )
                                (t line)
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun load-problems (filename)
    (cond
        (
            (not (stringp filename))
            nil
        )
        (T
            (remove-nil-from-list (create-list-from-file filename))
        )
    )
)

;; ler-algoritmo
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar. Em caso de input inválido assume-se a ultima opção"
    (progn
        (format t "Que algoritmo quer usar para procurar? ~%")
        (format t "1- Procura na largura ~%")
        (format t "2- Procura na profundidade ~%")
        (format t "3- A* ~%")
        (let 
            (
                (answer (read))
            )
            (cond 
                (
                    (= answer 1) 
                    'bfs-init
                )
                (
                    (= answer 2)
                    'dfs-init
                )
                (t
                    'a-star-init
                )
            )
        )
    )
)

;; ler-profundidade
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
        (format t "Qual a profundidade limite? ~%")
        (read)
    )
)

;; ler-profundidade
(defun choose-problem(filename)
"Permite escolher o problema a ser utilizado."
    (progn
        (format t "Qual a posição do problema? ~%")
        (let
            (
                (answer (read))
                (list-of-problems (load-problems filename))
            )
            (cond
                (
                    (or (not (numberp answer)) (> answer (list-length list-of-problems)) (< answer 1))
                    (progn
                        (format t "Essa posição não existe~%")
                        (choose-problem filename)
                    )
                )
                (T
                    (nth (1- answer) list-of-problems)
                )
            )
        )
    )
)

(defun ler-heuristica()
"Permite fazer a leitura da heuristica a utilizar para o algoritmo A*, em caso de input inválido assume-se a ultima opção"
    (progn
        (format t "1- Avaliação por caixas por fechar ~%")
        (format t "2- Avaliação por arcos por preencher ~%")
        (let
            (
                (answer (read))
            )
            (cond
                (
                    (= answer 1)
                    'heuristic-eval-by-remaining-to-close
                )
                (t
                    'heuristic-eval-by-remaining-arcs
                )
            )
        )
    )
)

(defun write-to-file-and-console (text stream)
    (format stream text)
    (format t text)
)

(defun main (filename)
"Inicio do programa"
    (progn
        (format T "Welcome to IA 0.1 :)~%")
        (let* 
            (
                (problem (choose-problem filename))
                (algoritmo (ler-algoritmo))
                (profundidade (cond ((eql algoritmo 'dfs-init) (ler-profundidade)) (T 5)))
                (heuristica (cond ((eql algoritmo 'a-star-init) (ler-heuristica)) (T 'heuristic-eval-by-remaining-to-close)))
            )
            (with-open-file (stream "desempenho.dat" :direction :output :if-does-not-exist :create :if-exists :append)
                (cond
                    (
                        (equal algoritmo 'bfs-init)
                        (progn
                            (write-to-file-and-console (format nil "Board: ~d~%Objetivo:~d~%Algoritmo: BFS~%" (car problem) (cadr problem)) stream)
                            (time
                                (let
                                    (
                                        (search-result (funcall algoritmo (car problem) (cadr problem)))
                                    )
                                    (write-to-file-and-console (format nil "Resultado da procura: ~d~%" (car search-result)) stream)
                                    (write-to-file-and-console (format nil "Total de nós gerados: ~d~%" (nth 1 search-result)) stream)
                                    (write-to-file-and-console (format nil "Total de nós expandidos: ~d~%" (nth 2 search-result)) stream)
                                    (write-to-file-and-console (format nil "Ramificação: ~d~%" (nth 3 search-result)) stream)
                                    (write-to-file-and-console (format nil "Tabuleiro solução: ~d~%" (caar search-result)) stream)
                                ) 
                            )
                        )
                    )
                    (
                        (equal algoritmo 'dfs-init)
                        (progn
                            (write-to-file-and-console (format nil "Board: ~d~%Objetivo:~d~%Algoritmo: DFS~%Profundidade: ~d~%" (car problem) (cadr problem) profundidade) stream)
                            (time
                                (let
                                    (
                                        (search-result (funcall algoritmo (car problem) (cadr problem) profundidade))
                                    )
                                    (write-to-file-and-console (format nil "Resultado da procura: ~d~%" (car search-result)) stream)
                                    (write-to-file-and-console (format nil "Total de nós gerados: ~d~%" (nth 1 search-result)) stream)
                                    (write-to-file-and-console (format nil "Total de nós expandidos: ~d~%" (nth 2 search-result)) stream)
                                    (write-to-file-and-console (format nil "Ramificação: ~d~%" (nth 3 search-result)) stream)
                                    (write-to-file-and-console (format nil "Tabuleiro solução: ~d~%" (caar search-result)) stream)
                                )
                            )
                        )
                    )
                    (
                        (equal algoritmo 'a-star-init)
                        (progn
                            (write-to-file-and-console (format nil "Board: ~d~%Objetivo:~d~%Algoritmo: A*~%" (car problem) (cadr problem)) stream)
                            (cond
                                (
                                    (equal heuristica 'heuristic-eval-by-remaining-arcs)
                                    (write-to-file-and-console (format nil "Heuristica: h(x) = nº de zeros na board~%") stream)
                                    (time
                                        (let
                                            (
                                                (search-result (funcall algoritmo (car problem) (cadr problem) heuristica))
                                            )
                                            (write-to-file-and-console (format nil "Resultado da procura: ~d~%" (car search-result)) stream)
                                            (write-to-file-and-console (format nil "Total de nós gerados: ~d~%" (nth 1 search-result)) stream)
                                            (write-to-file-and-console (format nil "Total de Nós expandidos: ~d~%" (nth 2 search-result)) stream)
                                            (write-to-file-and-console (format nil "Fator de Ramificação Médio: ~d~%" (nth 3 search-result)) stream)
                                            (write-to-file-and-console (format nil "Penetrância: ~d~%" (nth 4 search-result)) stream)
                                            (write-to-file-and-console (format nil "Tabuleiro solução: ~d~%" (caar search-result)) stream)
                                        )
                                    )  
                                )
                                (t
                                    (write-to-file-and-console (format nil "Heuristica: h(x) = o(x) - c(x)~%") stream)
                                    (time
                                        (let
                                            (
                                                (search-result (funcall algoritmo (car problem) (cadr problem) heuristica (cadr problem)))
                                            )
                                            (write-to-file-and-console (format nil "Resultado da procura: ~d~%" (car search-result)) stream)
                                            (write-to-file-and-console (format nil "Total de nós gerados: ~d~%" (nth 1 search-result)) stream)
                                            (write-to-file-and-console (format nil "Total de Nós expandidos: ~d~%" (nth 2 search-result)) stream)
                                            (write-to-file-and-console (format nil "Fator de Ramificação Médio: ~d~%" (nth 3 search-result)) stream)
                                            (write-to-file-and-console (format nil "Penetrância: ~d~%" (nth 4 search-result)) stream)
                                            (write-to-file-and-console (format nil "Tabuleiro solução: ~d~%" (caar search-result)) stream)
                                        )
                                    )
                                ) 
                            )
                        )
                    )    
                )
            )
        )
    )
)

