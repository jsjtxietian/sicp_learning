(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\queue_3.3.2.scm")


;;not so good 
(define (print-queue queue) 
    (define (iter lst)
        (if (null? lst)
            (display '())
            (begin 
                (display (car lst))
                (display " ")
                (iter (cdr lst))
            )
        )
    )

    (iter (front-ptr queue))
)

(define (print-queue queue) (car queue))