(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\poly_2.5.3.scm")


(define (install-polynomial-package)

    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (variable? x)   (symbol? x))
    (define (same-variable? v1 v2)
        (and
            (variable? v1)
            (variable? v2)
            (eq? v1 v2)))
    
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (make-term order coeff) (list order coeff))
    (define (the-empty-termlist) '())
    
    (define (make-poly-by-dense var terms)
        ((get 'make 'dense) var terms))
    (define (make-poly-by-sparse var terms)
        ((get 'make 'sparse) var terms))
            
    (define (add-terms L1 L2)
        (cond
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
                (let ((t1 (first-term L1)) (t2 (first-term L2)))
                    (cond
                        ((> (order t1) (order t2))
                            (adjoin-term t1
                                         (add-terms (rest-terms L1) L2)))
                        ((< (order t1) (order t2))
                            (adjoin-term t2
                                         (add-terms L1 (rest-terms L2))))
                        (else
                            (adjoin-term
                                (make-term (order t1)
                                           (add (coeff t1) (coeff t2)))
                                (add-terms (rest-terms L1)
                                           (rest-terms L2)))))))))
    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2)
                       (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term L)))
                (adjoin-term
                    (make-term (+ (order t1) (order t2))
                                (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms L))))))

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly-by-dense (variable p1)
                    (add-terms (term-list p1)
                                (term-list p2)))
            (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly-by-dense (variable p1)
                    (mul-terms (term-list p1)
                                (term-list p2)))
            (error "Polys not in same var -- MULT-POLY" (list p1 p2))))
   

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) 
        (lambda (p1 p2) (tag (mul-poly p1 p2))))

    (put 'make-poly-by-dense 'polynomial 
        (lambda (var terms) (tag (make-poly-by-dense var terms))))
    (put 'make-poly-by-sparse 'polynomial 
        (lambda (var terms) (tag (make-poly-by-sparse var terms))))

    (put '=zero? '(polynomial)
        (lambda (x) (empty-termlist? (term-list p))))

    (put 'sub '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))

    (put 'negate '(polynomial)
        (lambda (x) (tag (negate-poly x))))  
        
    'done
)

(define (install-polynomial-dense-package)

    (define (order term) (car term))
    (define (coeff term) (cadr term))

    (define (adjoin-term term term-list)
        (cond 
            ((=zero? (coeff term)) term-list)
            ((= (order term) (length term-list)) (cons (coeff term) term-list))
            (else
                (adjoin-term term (cons 0 term-list)))))

    (define (first-term term-list) 
        (make-term (- (length term-list) 1) (car term-list)))

    (define (tag x) 
        (attach-tag 'dense x))

    (put 'adjoin-term '(dense) 
         (lambda (term term-list) (tag (adjoin-term term term-list)))) 

    (put 'first-term '(dense) 
         (lambda (term-list) (tag (first-term term-list)))) 

    (put 'make 'dense
        (lambda (var terms) (tag (cons var terms))))        

'done
) 
   
(define (install-polynomial-sparse-package) 

    (define (order term) (car term))
    (define (coeff term) (cadr term))

    (define (adjoin-term term term-list) 
        (if (=zero? (coeff term)) 
            term-list 
            (cons term term-list)))

    (define (first-term term-list) (car term-list))

    (define (tag x) 
        (attach-tag 'sparse x))

    (put 'adjoin-term '(sparse) 
            (lambda (term term-list) (tag (adjoin-term term term-list))))

    (put 'first-term '(sparse) 
            (lambda (term-list) (first-term (tag term-list)))) 

    (put 'make 'sparse
        (lambda (var terms) (cons var (tag terms))))

'done
) 

(define (make-poly-by-dense var terms)
    ((get 'make-poly-by-dense 'polynomial) var terms))

(define (make-poly-by-sparse var terms)
    ((get 'make-poly-by-sparse 'polynomial) var terms))

(define (first-term term-list) (apply-generic 'first-term term-list))
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))

(install-polynomial-sparse-package)
(install-polynomial-dense-package)
(install-polynomial-package)

(define x (make-poly-by-dense 'x '(1 2 3 0 5)))