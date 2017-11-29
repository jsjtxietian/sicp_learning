(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\monte-carlo_3.1.2.scm")

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random high))))


(define (estimate-integral p? x1 x2 y1 y2 trials)
    (*  4
        (monte-carlo trials
                        (lambda ()
                            (p? (random-in-range x1 x2)
                                (random-in-range y1 y2))))))

(define (get-pi trials)
    (exact->inexact
        (estimate-integral (lambda (x y)
                            (< (+ (square x)
                                    (square y))
                                1.0))
                        -1.0
                        1.0
                        -1.0
                        1.0
                        trials)))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low 
            (random (exact->inexact range))
        )
    )
) 

