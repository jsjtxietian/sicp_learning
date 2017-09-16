(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a )
           (sum term (next a ) next b)
        )
    )
)

(define (inc x)
    (+ x 1)
)

(define (cube x)
    (* x x x)
)

(define (simpson f a b n)
    (define h
        (/ (- b a) n)
    )

    (define (y k)
        (f (+ a (* k h)))
    )

    (define (factor x)
        (cond
            ((or (= x 0) (= x n)) 1)
            ((odd? x) 4)
            (else 2)
        )
    )

    (define (term x)
        (* (y x) (factor x))
    )

    (if (not (even? n))
        (error "n can't br odd")
        (* (/ h 3) (sum term (exact->inexact 0) inc n))        
    )
)

(display (simpson cube 0 1 1000))