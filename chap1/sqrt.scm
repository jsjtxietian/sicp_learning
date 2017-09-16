(define (avr x y)
    (/ (+ x y ) 2)
)


(define (improve guess x)
    (avr guess (/ x guess))
)


(define (good? guess x)
;    (< (abs (- (* guess guess) x)) 0.001)
    (< (abs (- guess (improve guess x))) 0.001 )
)

(define (sqrt-iter guess x)
    (if (good? guess x)
        guess 
        (sqrt-iter (improve guess x  )
                    x))
)

(define (sqrt x)
    (sqrt-iter 1.0 x)
)

(display (sqrt  900000000000000000000000000000000000000000000000000000000000000000000000000000000000))
