(define (f n)
    (calc 2 1 0 0 n)
)

(define (calc a b c n count)
    (if (= n count)
        c
        (calc 
            (+ a (* 2 b) (* 3 c)) 
            a
            b
            (+ n 1)
            count 
        )
    )
)


(display (f 4))