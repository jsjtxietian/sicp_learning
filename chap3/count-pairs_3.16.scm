

(define (count-pairs x)
    (if (not (pair? x))
        0
        (+  (count-pairs (car x))
            (count-pairs (cdr x))
            1)
    )
)

(define three (cons (cons 1 '()) (cons 2 '())))
(display (count-pairs three))

(newline)

(define two (list 1 2))
(define four (cons two (cdr two)))
(display (count-pairs four))

(newline)

(define one (list 1))
(define three (cons one one))
(define seven (cons three three))
(display (count-pairs seven))



