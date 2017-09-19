(define us-coins
    (list 50 25 10 5 1)    
)

(define ck-coins
    (list 100 50 20 10 5 2 1 0.5)
)

(define (cc amount coin-values)
    (
        cond 
            ((= amount 0) 1)
            ((or (< amount 0) (no-more? coin-values)) 0)
            (else
                (+
                    (cc amount (except-first-denomination coin-values))
                    (cc (- amount (first-denomination coin-values)) coin-values)
                )
            ) 
    )
)

(define (except-first-denomination s)
    (cdr s)
)

(define (first-denomination s)
    (car s)
)

(define (no-more? x)
    (null? x)
)

(define (count-change amount)
    (cc amount us-coins)
)

(display (count-change 100))