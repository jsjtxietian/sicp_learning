(define (reverse lst)
    (iter lst '())
)

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
            (cons (car remained-items) result))
    )
)

(display (reverse (list 1 2 3)))