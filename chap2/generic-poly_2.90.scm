(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\poly_2.5.3.scm")


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
    
    (define (adjoin-term term term-list) 
        (let ((preped-term ((get 'prep-term (type-tag term-list)) term)) 
                (preped-first-term ((get 'prep-term (type-tag term-list)) 
                                    (first-term term-list)))) 
            (cond ((=zero? (coeff term)) term-list)  
                ((empty-termlist? term-list) (append (the-empty-termlist term-list)  
                                                        preped-term 
                                                        (zero-pad (order term) 
                                                                (type-tag 
                                                                    term-list)))) 
                ((> (order term) (order (first-term term-list))) 
                    (append (list (car term-list)) 
                            preped-term  
                            (zero-pad (- (- (order term) 
                                            (order (first-term term-list))) 
                                        1) (type-tag term-list)) 
                            (cdr term-list))) 
                (else 
                    (append preped-first-term  
                            (adjoin-term term (rest-terms term-list)))))))

    (define (first-term term-list)
        (list (- (length term-list) 1) (car term-list)))


    (define (rest-terms term-list) 
        (let ((proc (get 'rest-terms (type-tag term-list)))) 
            (if proc 
                (proc term-list) 
                (error "-- REST-TERMS" term-list)))) 
    (define (empty-termlist? term-list) 
        (let ((proc (get 'empty-termlist? (type-tag term-list)))) 
            (if proc 
                (proc term-list) 
                (error "-- EMPTY-TERMLIST?" term-list)))) 
    (define (make-term order coeff) (list order coeff)) 
    (define (order term) 
        (if (pair? term) 
            (car term) 
            (error "Term not pair -- ORDER" term))) 
    (define (coeff term) 
        (if (pair? term) 
            (cadr term) 
            (error "Term not pair -- COEFF" term))) 
    (define (the-empty-termlist term-list) 
        (let ((proc (get 'the-empty-termlist (type-tag term-list)))) 
        (if proc 
            (proc) 
            (error "No proc found -- THE-EMPTY-TERMLIST" term-list)))) 
    
            
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
            (make-poly (variable p1)
                    (add-terms (term-list p1)
                                (term-list p2)))
            (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                    (mul-terms (term-list p1)
                                (term-list p2)))
            (error "Polys not in same var -- MULT-POLY" (list p1 p2))))
   
    (define (negate p) 
    (let ((neg-p ((get 'make-polynomial (type-tag (term-list p))) 
                  (variable p) (list (make-term 0 -1))))) 
        (mul-poly (cdr neg-p) p)))

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) 
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial 
        (lambda (var terms) (tag (make-poly var terms))))

    (put '=zero? '(polynomial)
        (lambda (x) (empty-termlist? (term-list p))))
    (put 'sub '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
    (put 'negate '(polynomial)
        (lambda (x) (tag (negate x))))  
        
    'done
)
  
(define (first-term term-list) 
    (let ((proc (get 'first-term (type-tag term-list)))) 
        (if proc 
            (proc term-list) 
            (error "No first-term for this list -- FIRST-TERM" term-list)))) 


(define (install-polynomial-term-package) 
    (define (first-term-dense term-list) 
        (if (empty-termlist? term-list) 
            '() 
            (list 
            (- (length (cdr term-list)) 1) 
            (car (cdr term-list)))))

    (define (first-term-sparse term-list) 
        (if (empty-termlist? term-list) 
            '() 
            (cadr term-list)))

    (define (prep-term-dense term) 
        (if (null? term) 
            '() 
            (cdr term)))
                                
    (define (prep-term-sparse term) 
        (if (null? term) 
            '() 
            (list term)))
                             
    (define (the-empty-termlist-dense) '(dense)) 
    (define (the-empty-termlist-sparse) '(sparse)) 
    (define (rest-terms term-list) (cons (type-tag term-list) (cddr term-list))) 
    (define (empty-termlist? term-list)  
        (if (pair? term-list)  
            (>= 1 (length term-list)) 
            (error "Term-list not pair -- EMPTY-TERMLIST?" term-list))) 
    (define (make-polynomial-dense var terms) 
        (make-polynomial var (cons 'dense (map cadr terms)))) 
    (define (make-polynomial-sparse var terms) 
        (make-polynomial var (cons 'sparse terms))) 
    (put 'first-term 'sparse  
        (lambda (term-list) (first-term-sparse term-list))) 
    (put 'first-term 'dense 
        (lambda (term-list) (first-term-dense term-list))) 
    (put 'prep-term 'dense 
        (lambda (term) (prep-term-dense term))) 
    (put 'prep-term 'sparse 
        (lambda (term) (prep-term-sparse term))) 
    (put 'rest-terms 'dense 
        (lambda (term-list) (rest-terms term-list))) 
    (put 'rest-terms 'sparse 
        (lambda (term-list) (rest-terms term-list))) 
    (put 'empty-termlist? 'dense 
        (lambda (term-list) (empty-termlist? term-list))) 
    (put 'empty-termlist? 'sparse 
        (lambda (term-list) (empty-termlist? term-list))) 
    (put 'the-empty-termlist 'dense 
        (lambda () (the-empty-termlist-dense))) 
    (put 'the-empty-termlist 'sparse 
        (lambda () (the-empty-termlist-sparse))) 
    (put 'make-polynomial 'sparse 
        (lambda (var terms) (make-polynomial-sparse var terms))) 
    (put 'make-polynomial 'dense 
        (lambda (var terms) (make-polynomial-dense var terms))) 
'done
) 

(install-polynomial-term-package)     
(install-polynomial-package)
 
 
; I had to changhe the adjoin-term procedure. It now does  
; zero padding so we can `mul` dense polynomials correctly.  
 
(define (zero-pad x type) 
    (if (eq? type 'sparse) 
        '() 
        (if (= x 0) 
            '() 
            (cons 0 (add-zeros (- x 1)))))) 
 
 
 

 