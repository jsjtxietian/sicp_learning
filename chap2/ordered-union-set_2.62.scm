(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\ordered-adjoin-set_2.61.scm")

(define (union-set set1 set2)
    (define (iter s1 s2 result)
        (cond
            ((null? s1) (append (reverse s2) result))
            ((null? s2) (append (reverse s1) result))
            (else
                (let
                    ((x1 (car s1))  (x2 (car s2))
                        (r1 (cdr s1))   (r2 (cdr s2)))
                    (cond
                        ((= x1 x2)  (iter r1 r2 (cons x1 result)))
                        ((< x1 x2)  (iter r1 s2 (cons x1 result)))
                        ((> x1 x2)  (iter s1 r2 (cons x2 result)))
                    )
                )
            )
        )
    )
    (reverse   (iter set1 set2 '()))
)

(define (union-set set another)
    (cond 
        ((and (null? set) (null? another))
            '())
        ((null? set)
            another)
        ((null? another)
            set)
        (else
            (let ((x (car set)) (y (car another)))
                (cond ((= x y)
                        (cons x (union-set (cdr set) (cdr another))))
                    ((< x y)
                        (cons x (union-set (cdr set) another)))
                    ((> x y)
                        (cons y (union-set set (cdr another))))))
        )
    )
)


;(define (adjoin-set x set)  (union-set (list x) set))