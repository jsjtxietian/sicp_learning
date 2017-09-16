(define (cont-frac N D k)
    (define (cf i)
        (if ( = k i)
            (/ (N k) (D k))
            (/ (N i) (+ (D i) (cf (+ i 1))))
        )
    )
    (cf 1)
)

(define (iter N D k)
    (define (fuck i result)
        (if (= i 0) result
            (fuck (- i 1) (/ (N i) (+ (D i) result)))
        )
    )
    (fuck k 0)
)

(define (golden-ratio k)
    (+ 1
        (iter (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k)
    )
)

(define (e k)
    (define (N i)
        1
    )
    (define (D i)
        (if (= 0 (remainder (+ i 1) 3))
            (* 2 (/ (+ i 1) 3))
            1
        )
    )

    (+ 2.0 
       (cont-frac N D k))
)

(define (square x)
    (* x x)
)

(define (tan-cf x k)
    (define (N i)
        (if (= i 1)
            x
            (- (square x))))

    (define (D i)
        (- (* i 2) 1)
    )
    (exact->inexact (cont-frac N D k))
)

(display (tan-cf 25 1000))
(newline)
(display (tan 25))