(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (map proc (cdr items)))
    )
)

(define (square-list items)
    (map
        (lambda (x) (* x x))
        items 
    )
)


(display (square-list (list 1 2 3 4)))
