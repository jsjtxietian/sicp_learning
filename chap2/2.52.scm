;a
(segments->painter 
    (list 
        (make-segment (make-vect 0.0 0.0)
        (make-vect 1.0 1.0))

        ; ... wave 图形的线段
        )
)

;b
(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\up-split_2.44.scm")
(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
            (beside (below painter up)
                    (below right corner))))
)

;c
(define (square-limit painter n)
    (let ((combine4 (square-of-four identity flip-horiz)
                                    flip-vect rotate180))
        (combine4 (corner-split painter n))
    )
)