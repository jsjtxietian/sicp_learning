(define (close-enough? x y)
    (< (abs (- x y)) 0.001)
)

(define (negtive? x)
    (< x 0)
)

(define (postive? x)
    (> x 0)
)

(define (search f neg-point pos-point)
    (let
        ((midpoint (average neg-point pos-point))) 
        (if (close-enough? neg-point pos-point)
            midpoint
            (let 
                ((test-value (f midpoint)))
                (cond 
                    ((postive? test-value) (search f neg-point midpoint))
                    ((negtive? test-value) (search f midpoint pos-point))
                    (else midpoint)
                )
            )
        )
    )
)

(define (half-interval-method f a b)
    (let
        ((a-value (f a))
         (b-value (f b))
        )

        (cond 
            ((and (negtive? a-value) (postive? b-value)) (search f a b))
            ((and (negtive? b-value) (postive? a-value)) (search f b a))
            (else (error "fuck"))
        )
    )
)

(display (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0))