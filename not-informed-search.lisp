(defpackage :not-informed-search
    (:use
        #:COMMON-LISP
        #:node
    )
    (:export
        #:bfs-init
        #:bfs
        #:add-to-open-list-bfs
        #:get-objective-node
        #:node-existp
        #:get-successors-not-in-closed
    )
)

(in-package :not-informed-search)

(defun bfs-init (start-node closed-boxes-objective)
    (funcall 'bfs closed-boxes-objective (list start-node))
)

(defun bfs (closed-boxes-objective OPEN-LIST &optional CLOSED-LIST)
    (cond
        (
            (null OPEN-LIST)
            NIL
        )
        (T
            (let 
                (
                    (successors (funcall 'get-successors (car OPEN-LIST)))
                )
                (let
                    (
                        (objective-node (funcall 'get-objective-node closed-boxes-objective successors))
                    )
                    (cond
                        (
                            (null objective-node)
                            (bfs
                                closed-boxes-objective
                                (funcall 'add-to-open-list-bfs
                                    (cdr OPEN-LIST)
                                    (funcall 'get-successors-not-in-closed successors (append OPEN-LIST CLOSED-LIST))
                                )
                                (append (list (car OPEN-LIST)) CLOSED-LIST)
                            )
                        )
                        (T
                            objective-node
                        )
                    )
                )
            )
        )
    )
)

(defun add-to-open-list-bfs (OPEN-LIST successors-list)
    (cond
        (
            (and (null OPEN-LIST) (null successors-list))
            NIL
        )
        (
            (null OPEN-LIST)
            successors-list
        )
        (
            (null successors-list)
            OPEN-LIST
        )
        (T
            (append OPEN-LIST successors-list)
        )
    )
)

(defun get-successors-not-in-closed (successors CLOSED-LIST)
    (cond
        (
            (null successors)
            NIL
        )
        (
            (funcall 'node-existp (car successors) CLOSED-LIST)
            (get-successors-not-in-closed (cdr successors) CLOSED-LIST)
        )
        (T
            (cons (car successors) (get-successors-not-in-closed (cdr successors) CLOSED-LIST))
        )
    )
)

(defun node-existp (node compare-list)
    (cond
        (
            (null compare-list)
            NIL
        )
        (
            (equal (funcall 'get-node-state node) (funcall 'get-node-state (car compare-list)))
            T
        )
        (
            (node-existp node (cdr compare-list))
        )
    )
)

(defun get-objective-node (closed-boxes-objective successors)
    (cond
        (
            (null successors)
            NIL
        )
        (
            (funcall 'objective-node (car successors) closed-boxes-objective)
            (car successors)
        )
        (
            (get-objective-node closed-boxes-objective (cdr successors))
        )
    )
)