(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\even-fibs_2.2.3.scm")

(define (horner-eval x coefficient-sequences)
    (accumulate 
        (lambda (a b) 
            (+ a (* b x) )
            )
        0
        coefficient-sequences
    )
)

(display (horner-eval 2 (list 1 3 0 5 0 1)))

;1 + 3x + 5x^3 + x ^5 = 79