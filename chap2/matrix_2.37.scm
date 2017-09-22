(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\accumulate-n_2.36.scm")

(define (dot-product v w)
    (accumulate
        +
        0
        (map * v w)
    )
)

(define (matrix-*-vector m v)
    (map
        (lambda (col) (dot-product col v))
        m
    )
)

(define (transpose mat)
    (accumulate-n
        cons
        '()
        mat
    )
)

(define (matrix-*-matrix m n)
    (let ((trans-n (transpose n)))
        (map 
            (lambda (col-of-m)
                (matrix-*-vector trans-n col-of-m))
            m
        )
    )
)

(define m 
    (list 
        (list 1 2 3 4)
        (list 4 5 6 6)
        (list 6 7 8 9)
    )
)

(display (matrix-*-matrix m (transpose m)))