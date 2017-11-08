(define (list-walk lst step)
    (cond 
        ((null? lst) '())
        ((= step 0) lst)
        (else (list-walk (cdr lst) (- step 1)))
    )
)

(define (loop? lst)
    (define (iter x y)
        (let ((x-pointer (list-walk x 1))
                (y-pointer (list-walk y 2)))
            (cond
                ((or (eq? x-pointer '()) 
                    (eq? y-pointer '())) 
                    #f)
                ((eq? x-pointer y-pointer) #t)
                (else (iter x-pointer y-pointer))
            ) 
        )
    )

    (iter lst lst)
)

