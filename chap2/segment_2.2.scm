(define (make-segment x y)
    (cons x y)
)

(define (start-segment s)
    (car s)
)

(define (end-segment s)
    (cdr s)
)

(define (make-point x y)
    (cons x y)
)

(define (x-point p)
    (car p)
)

(define (y-point p)
    (cdr p)
)

(define (print-point p)
    ;(newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (midpoint-segment s)
    (let
        (
            (x1 (x-point (start-segment s)))
            (y1 (y-point (start-segment s)))
            (x2 (x-point (end-segment s)))
            (y2 (y-point (end-segment s)))
        )

        (make-point (average x1 x2) (average y1 y2))
    )
)

(define p
    (make-segment (make-point 1 2) (make-point 3 4))
)


;(print-point (midpoint-segment p))