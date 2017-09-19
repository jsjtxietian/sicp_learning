( define (for-each p lst)
    (if (not (null? lst))
        (begin
            (p (car lst))
            (for-each p (cdr lst))))
)

(for-each (lambda (x) (display x)) (list 1 2 3 4))