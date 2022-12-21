(defpackage :not-informed-search
    (:use
        #:COMMON-LISP
        #:node
    )
    (:export
        #:bfs-init
        #:dfs-init
        #:dfs
        #:bfs
        #:generate-sucessors-dfs
        #:add-to-open-list-bfs
        #:get-objective-node
        #:node-existp
        #:get-successors-not-in-closed
        #:add-to-open-list-dfs
    )
)

(in-package :not-informed-search)

(defun dfs-init (start-node closed-boxes-objective max-depth)
    (funcall 'dfs closed-boxes-objective max-depth (list start-node) NIL 0)
)

(defun dfs (closed-boxes-objective max-depth OPEN-LIST &optional CLOSED-LIST number-of-generated-nodes)
    (cond
        (
            (null OPEN-LIST)
            (list NIL number-of-generated-nodes)
        )
        (T
            (let
                (
                    (successors (funcall 'generate-sucessors-dfs (car OPEN-LIST) max-depth))
                )
                (cond
                    (
                        (null successors)
                        (dfs
                            closed-boxes-objective
                            max-depth
                            (funcall 'add-to-open-list-dfs
                                (cdr OPEN-LIST)
                                NIL
                            )
                            (append (list (car OPEN-LIST)) CLOSED-LIST)
                            number-of-generated-nodes
                        )
                    )
                    (T
                        (let
                            (
                                (objective-node (funcall 'get-objective-node closed-boxes-objective successors))
                            )
                            (cond
                                (
                                    (null objective-node)
                                    (dfs
                                        closed-boxes-objective
                                        max-depth
                                        (funcall 'add-to-open-list-dfs
                                            (cdr OPEN-LIST)
                                            (funcall 'get-successors-not-in-closed successors (append OPEN-LIST CLOSED-LIST))
                                        )
                                        (append (list (car OPEN-LIST)) CLOSED-LIST)
                                        (+ number-of-generated-nodes (list-length successors))
                                    )
                                )
                                (T
                                    (list objective-node (+ number-of-generated-nodes (list-length successors)))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun bfs-init (start-node closed-boxes-objective)
    (funcall 'bfs closed-boxes-objective (list start-node) NIL 0)
)

(defun bfs (closed-boxes-objective OPEN-LIST &optional CLOSED-LIST number-of-generated-nodes)
    (cond
        (
            (null OPEN-LIST)
            (list NIL number-of-generated-nodes)
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
                                (+ number-of-generated-nodes (list-length successors))
                            )
                        )
                        (T
                            (list objective-node (+ number-of-generated-nodes (list-length successors)))
                        )
                    )
                )
            )
        )
    )
)

(defun generate-sucessors-dfs (node max-depth)
    (let
        (
            (successors (funcall 'get-successors node))
        )
        (cond
            (
                (null successors)
                NIL
            )
            (
                (> (funcall 'get-node-depth (car successors)) max-depth)
                NIL
            )
            (
                successors
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

(defun add-to-open-list-dfs (OPEN-LIST successors-list)
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
            (append successors-list OPEN-LIST)
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