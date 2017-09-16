(define (double parameters)
    (lambda (x) 
        (parameters (parameters x))
    )
)

(define (inc x)
    (+ x 1)
)

(define dx 0.00001)

(define (square x)
    (* x x)
)

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)

(define (repeated g n)
    (if (= n 1)
        g
        (compose (repeated g (- n 1)) g)
    )
)

(define (smooth f)
    (lambda (x)
        (/ (+ (f x) 
            (f (+ x dx)) 
            (f (- x dx))) 
           3)
    )
)

(define (smooth-n-times f n)
    (let ((n-times-smmoth (repeated smooth n)))
        (n-times-smmoth f)
    )
)

(display  ((smooth-n-times square 10) 5))