(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\apply-generic_2.84.scm")

(define (drop x)
    (if (= 1 (level (type-tag x)))
        x
        (if (equal? x (raise (project x)))
            (drop (project x))
            x
        )
    )
)

;(define (project x) ((get 'project (list (type-tag x))) (contents x)))
(define (project x) (apply-generic 'project x))
(define (raise x) ((get 'raise (list (type-tag x))) (contents x)))


(define (apply-generic op . args) 
    (let ((type-tags (map type-tag args))) 
        (define (no-method) 
            (error "No method for these types" (list op type-tags))
        ) 
        (let ((proc (get op type-tags))) 
            (if proc
                (if (need-drop op)
                    (drop (apply proc (map contents args)))                                            
                    (apply proc (map contents args))
                )
                (if (not (null? (cdr args))) ; length of args > 1 
                    (let ((raised-args (raise-to-common args))) 
                        (if raised-args 
                            (let ((proc (get op (map type-tag raised-args)))) 
                                (if proc
                                    (if (need-drop op)
                                        (drop (apply proc (map contents raised-args)))
                                        (apply proc (map contents raised-args))
                                    ) 
                                    (no-method))) 
                        (no-method))) 
                    (no-method)
                )
            )
        )
    )
)

(define (need-drop x) 
    (or
        (eq? x 'add)
        (eq? x 'sub)
        (eq? x 'mul)
        (eq? x 'div)
    )    
)

(define p (make-scheme-number 2))
(define q (make-rational 3 1))
(define m (make-complex-from-real-imag 3 0))
