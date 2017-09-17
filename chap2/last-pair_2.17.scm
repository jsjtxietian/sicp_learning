(define (list-ref items n) 
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))
    )
)

(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items))) 
    )
)

(define (length items)
    (define (iter a count)
        ( if (null? a)
            count
            (iter (cdr a ) (+ 1 count ))
        )
    )
    (iter items 0)
)

(define (append a b)
    ( if (null? a)
        b
        (cons (car a) (append (cdr a) b))
    )
)

(define (last-pair a)
    (list-ref a (- (length a) 1))
)

(define (last-pair a)
    (if (null? a)
        nil
        (if(null? (cdr a))
            (car a)
            (last-pair (cdr a))
        )
    )
)

(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))

(display (last-pair odds))