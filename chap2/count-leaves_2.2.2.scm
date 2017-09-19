(define x (cons (list 1 2) (list 3 4)))

(define (length x)
    (if(null? x)
        0
        (+ 1 (length (cdr x)))
    )
)

(define (count-leaves x)
    (cond 
        ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+
                (count-leaves (car x))    
                (count-leaves (cdr x)))
        )
    )
)

