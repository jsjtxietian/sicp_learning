(define (make-monitored f)
    (let ((count 0))
        (lambda (m) 
            (cond 
                ((eq? m 'reset-count) (set! count 0))
                ((eq? m 'how-many-calls?) count)
                (else (begin (set! count (+ count 1))
                        (f m)))    
            )    
        )
    )
)



(define s (make-monitored sqrt))

(display (s 100))
(display (s 'how-many-calls?))
