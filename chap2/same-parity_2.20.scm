(define (same-parity x . y)
    (define (iter list result)
        (if (null? list)
            result
            ( if (same x (car list))
                 (iter (cdr list) (cons (car list) result ))
                 (iter (cdr list) result)
            )
        )
    )

    (iter y nil)
)

(define (same x y)
    (= (remainder x 2) (remainder y 2))
)

(display (same-parity 2 1 2 3 4 5))