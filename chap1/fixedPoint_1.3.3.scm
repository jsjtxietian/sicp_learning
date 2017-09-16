(define tolerance 0.000001)

(define (fixed_point f firstguess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
    )
    (define (try guess step)
        (display-info guess step)
        (let
            ((next (f guess)))
            (if (close-enough? guess next)
                (begin
                    (display-info next (+ 1 step))
                    next
                )
                (try next (+ 1 step))
            )
        )    
    )   
    
    (try firstguess 1)
)

(define (display-info guess step)
    (display "Step: ")
    (display step)
    (display " ")

    (display "Guess: ")
    (display guess)
    (newline)
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (average-damp f)
    (lambda (x)
        (average x (f x))
    )
)


(define (sqrt x)
    (fixed_point (lambda (y) (average y (/ x y))) 1.0)
)

(define golden-ratio
    (fixed_point (lambda (x) (+ 1 (/ 1 x))) 1.0)
)

(define formula 
    (lambda (x)
        (/ (log 1000) 
           (log x)
        )
    )
)


;(display (fixed_point (average-damp formula) 2.0))
