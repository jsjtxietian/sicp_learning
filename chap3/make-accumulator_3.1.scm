(define (make-accumulator init)
    (lambda (amount)
        (set! init (+ init amount))
        init    
    )
)

(define A (make-accumulator 5))