(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\raise_2.83.scm")

(define (level type)
    (cond
        ((eq? type 'scheme-number) 1)
        ((eq? type 'rational) 2)
        ((eq? type 'complex) 3)
        (else (error "No such type!" type))
    )
)

(define (apply-generic op . args) 
    (let ((type-tags (map type-tag args))) 
        (define (no-method) 
            (error "No method for these types" (list op type-tags))
        ) 
        (let ((proc (get op type-tags))) 
            (if proc 
                (apply proc (map contents args)) 
                (if (not (null? (cdr args))) ; length of args > 1 
                    (let ((raised-args (raise-to-common args))) 
                    (if raised-args 
                        (let ((proc (get op (map type-tag raised-args)))) 
                            (if proc 
                                (apply proc (map contents raised-args)) 
                                (no-method))) 
                        (no-method))) 
                    (no-method)
                )
            )
        )
    )
) 

(define (raise-to-common args) 
    (let ((raised-args 
           (map (lambda (x) (raise-to-type (highest-type args) x)) 
                args))) 
        (if (all-true? raised-args) 
            raised-args 
            false
        )
    )
) 

(define (all-true? args)
    (cond
        ((null? args) #t)
        ((car args) (all-true? (cdr args)))
        (else #f)    
    )
)

(define (highest-type args)
    (define (iter current left)
        (cond
            ((null? left) current)
            ((< (level current) (level (type-tag (car left)))) (iter (type-tag (car left)) (cdr left)))
            (else (iter current (cdr left)))
        )
    )
    (iter (type-tag (car args)) (cdr args))
)

(define (raise-to-type type item) 
    (let ((item-type (type-tag item))) 
        (if (eq? item-type type) 
            item 
            (raise-to-type type (raise item))
        )
    )
) 


