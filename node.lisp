;;;; node.lisp
;;;; Possuí funções para consulta e criação de estados e valores de estados e seletores para os nós
;;;; Autor: Lucas Freixieiro e João Portas

(defpackage :node
    (:use
        #:COMMON-LISP
    )
    (:export
        #:new-node
        #:get-horizontal-arcs
        #:get-vertical-arcs
        #:get-arc-in-position
        #:get-connections-row-in-orientation-list
        #:replace-arc-connection
        #:replace-arc-connection-in-position
        #:draw-horizontal-arc
        #:draw-vertical-arc
    )
)

(in-package :node)

(defun new-node (state depth &optional (parent-node NIL))
    (list state depth parent-node)
)

(defun get-horizontal-arcs (board)
"Recebe um tabuleiro e retorna uma lista com sublistas em que cada representa um 'linha' horizontal do tabuleiro.
Parameters >>
    board (list): Representa um tabuleiro de jogo.
        exemplo:
        (
            ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
            ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
        )
Return <<
    (list): Um lista com as infromações dos arcos horizontais.
        exemplo:
        ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
    OR
    NIL: Se os parametros recebidos forem NULL.
    

"
	(cond
		(
			(null board)
			nil
		)
		(T
			(car board)
		)
	)
)

(defun get-vertical-arcs (board)
"Recebe um tabuleiro e retorna uma lista com sublistas em que cada representa um 'linha' vertical do tabuleiro.
Parameters >>
    board (list): Representa um tabuleiro de jogo.
        exemplo:
        (
            ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
            ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
        )
Return <<
    (list): Um lista com as infromações dos arcos verticais.
        exemplo:
        ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
    OR
    NIL: Se os parametros recebidos forem NULL.
    

"
	(cond
		(
			(null board)
			nil
		)
		(T
			(cadr board)
		)
	)
)

(defun get-arc-in-position (orientation-list row position)
"Obtem o estado de se de um ponto ao outro recebido numa determinada orientação estão ligados. Se for 1 tem uma linha a fechar os 2 pontos
caso contrário não tem uma linha a fechar esses 2 pontos
Parameters >>
    row (number): O número que corresponte à posição de um sub-lista numa lista de ligações horizontais ou verticais.
    position (number): O número que corresponde à posição de uma ligação entre dois pontos relativamente á lista de ligações (horizontai ou verticais) recebida e ao número do row escolhido.
    orientation-list (list): Lista relativamente à ligações horizontais ou vertiacas da qual vai ser extraida a informação.
Return:
    1: Se está uma linha a ligar 2 pontos
    0: Se não estão ligados um ao outro
"
	(cond
		(
			(or (null orientation-list) (< row 1) (< position 1))
			nil
		)
		(T
			(nth (- position 1) (nth (- row 1) orientation-list))
		)
	)
)

(defun get-connections-row-in-orientation-list (orientation-list row)
"Recebe uma lista ligações ver verticais ou horizontais e obtem uma linha da lista.
Parameters >>
    orientation-list (list): Um lista com as ligações verticais ou horizontais.
    row (number): O número da linhas das ligações a retornar.
Return <<
    (x y z) -> Em que cada letra corresponde naquela tinha à ligação entre os pontos.
"
    (cond
        (
            (null orientation-list)
            NIL
        )
        (T
            (nth (- row 1) orientation-list)
        )
    )
)

(defun replace-arc-connection (orientation-list-row position &optional (x 1))
"Recebe a posição da lista de conecxões de um corredor horizontal ou vertical e o corredor e substitui por um valor.
Parameters >>
    position (number): Posição numa lista com os estados das linhas num corredor ex: (1 0 1)
    orientation-list-row (list): Corredor de uma lista de horizontais ou verticais. ex: (1 0 1)
    [optional]
    x (number): Valor pelo qual vai ser substituido na lista recebida e na posição indicada.
Return <<
    (A B C) (list): Com o valor na posição indicada supstituido.
"
	(cond
		(
			(or (null orientation-list-row) (< position 1))
			nil
		)
		(
			(= position 1)
			(cons x (cdr orientation-list-row))
		)
		(T
			(cons (car orientation-list-row) (replace-arc-connection (cdr orientation-list-row) (- position 1) x))
		)
	)
)

(defun replace-arc-connection-in-position (orientation-list row position  &optional (x 1))
"Recebe a posição da lista de conecxões horizontal ou vertical, o corredor dentro da lista das conexções (horizontai ou verticias) e a posição em que se vai mudar o valor.
Parameters >>
    position (number): Posição numa lista com os estados das linhas num corredor ex: (1 0 1)
    orientation-list-row (list): Corredor de uma lista de horizontais ou verticais. ex: (1 0 1)
    [optional]
    x (number): Valor pelo qual vai ser substituido na lista recebida e na posição indicada.
Return <<
    (A B C) (list): Com o valor na posição indicada supstituido.
"
	(cond
		(
			(or (null orientation-list) (< row 1) (< position 1) (null (get-arc-in-position orientation-list row position)))
			nil
		)
		(
			(= row 1)
			(cons (replace-arc-connection (car orientation-list) position x) (cdr orientation-list))
		)
		(T
			(cons (car orientation-list) (replace-arc-connection-in-position (cdr orientation-list) (- row 1) position x))
		)
	)
)

(defun draw-horizontal-arc (board row position &optional (x 1))
"Altera a lista de horizontai para colocar uma linha horizontal na posição indicada.
Parameters >>
    board (list): Tabuleiro de jogo
    row (number): Numero da linha dos horizontais em que vai ser feita a alteração
    position (number): Numero do ponto que vai ligao ao seu vizinho á direita.
Return <<
    (list): Tabuleiro de jogo com a alteração feita.
"
	(cond
		(
			(or (null board) (< row 1) (< position 1) 
				(null (get-arc-in-position (get-horizontal-arcs board) row position))
				(/= (get-arc-in-position (get-horizontal-arcs board) row position) 0)
			)
			nil
		)
		(T
			(list
				(replace-arc-connection-in-position (get-horizontal-arcs board) row position x)	  
				(get-vertical-arcs board)
			)
		)
	)
)

(defun draw-vertical-arc (board row position &optional (x 1))
	(cond
		(
			(or (null board) (< row 1) (< position 1) 
				(null (get-arc-in-position (get-vertical-arcs board) row position))
				(/= (get-arc-in-position (get-vertical-arcs board) row position) 0)
			)
			nil
		)
		(T
			(list
				(get-horizontal-arcs board)
				(replace-arc-connection-in-position (get-vertical-arcs board) row position x)	  
			)
		)
	)
)