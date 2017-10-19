(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\drop_2.85.scm")




(define (install-polynomial-package)

    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (variable? x)   (symbol? x))
    (define (same-variable? v1 v2)
        (and
            (variable? v1)
            (variable? v2)
            (eq? v1 v2)))
            
    (define (add-terms L1 L2)
        (cond
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
                (let ((t1 (first-term L1) (t2 (first-term L2))))
                    (cond
                        ((> (order t1) (order t2))
                            (adjoin-term t1
                                         (add-terms (rest-terms L1) L2)))
                        ((< (order t1) (order t2))
                            (adjoin-term t2)
                                         (add-terms L1 (rest-terms L2)))
                        (else
                            (adjoin-term
                                (make-term (order t1)
                                           (add (ceoff t1) (ceoff t2)))
                                (add-terms (rest-terms L1)
                                           (rest-terms L2)))))))))
    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2)
                       (mul-terms (rest-terms L1) L2))))
    ()

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                    (add-terms (term-list p1)
                                (term-list p2)))
            (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                    (mult-terms (term-list p1)
                                (term-list p2)))
            (error "Polys not in same var -- MULT-POLY" (list p1 p2))))

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) 
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial 
        (lambda (var terms) (tag (make-poly var terms))))
    'done
)

;;;删掉了drop
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