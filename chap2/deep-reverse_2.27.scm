(define x (list (list 1 2) (list 3 4)))

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

(define (deep-reverse tree)
    ( cond
        ((null? tree) '())
        ((not (pair? tree)) tree)
        (else
            (reverse 
                (list 
                    (deep-reverse (car tree))
                    (deep-reverse (cadr tree))
                )
            )
            
        )
    )
)


(display (deep-reverse x))
(newline)
(display x)
(newline)
(display (car x))
(newline)
(display (cdr x))
