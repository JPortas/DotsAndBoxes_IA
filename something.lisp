(defun alphabeta-recursive (node alpha beta)
    (if 
        (is-leaf node)
            (static-evaluation node)
        (let ((children (children node)))
            (if 
                (is-max-node node)
                (reduce (lambda (v child)
                    (let ((result (alphabeta-recursive child alpha beta)))
                        (if (>= beta (cdr result))
                            (values beta alpha)
                            (values (max v (car result)) (max alpha (car result))))))
                    children
                    :initial-value (values -inf.0 alpha)
                )
                (reduce (lambda (v child)
                    (let
                        (
                            (result (alphabeta-recursive child alpha beta))
                        )
                        (if
                            (<= alpha (cdr result))
                                (values alpha beta)
                            (values (min v (car result)) (min beta (car result)))
                        )
                    ))
                    children
                    :initial-value (values inf.0 beta)
                )
            )
        )
    )
)
