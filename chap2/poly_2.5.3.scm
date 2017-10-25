;;包括2.87-2.89

(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\scheme-number_2.78.scm")

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
        (cond 
            ((=zero? (coeff term)) term-list)
            ((= (order term) (length term-list)) (cons (coeff term) term-list))
            (else
                (adjoin-term term (cons 0 term-list)))))

    (define (first-term term-list)
        (list (- (length term-list) 1) (car term-list)))


    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))
    (define (the-empty-termlist) '())

    (define (negate-term-list t)
        (map
            (lambda (term) (negate term))
            t))

    (define (negate-poly p)
        (make-poly (variable p)
            (negate-term-list (term-list p))))
            
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
    
    (define (div-poly p1 p2) 
        (if (same-variable? (variable p1) (variable p2)) 
            (let ((result (div-terms (term-list p1) 
                                    (term-list p2)))) 
                (list (make-poly (variable p1) 
                                (car result)) 
                    (make-poly (variable p1) 
                                (cadr result)))) 
            (error "Variable is not the same -- DIV-POLY" (list (variable p1) (variable p2))))) 

    (define (div-terms l1 l2)  
        (if (empty-termlist? l1) 
          (list (the-empty-termlist) (the-empty-termlist)) 
          (let ((t1 (first-term l1)) 
                (t2 (first-term l2))) 
            (if (> (order t2) (order t1)) 
              (list (the-empty-termlist) l1) 
              (let ((first-term (make-term (- (order t1) (order t2)) 
                                           (div (coeff t1) (coeff t2))))) 
                (let ((rest-of-result 
                        (div-terms (add-terms l1 
                                              (negate-term-list (mul-term-by-all-terms first-term 
                                                                     l2))) 
                                   l2))) 
                  (list (adjoin-term first-term (car rest-of-result)) 
                        (cadr rest-of-result)))))))) 


    
    ;;稀疏表示
    ; (define (negate-poly p)
    ;     (make-poly (variable p)
    ;         (map 
    ;             (lambda (term) 
    ;                 (make-term (order term) (negate (coeff term)))) 
    ;             (term-list p))))

    ; (define (adjoin-term term term-list)
    ;     (if (=zero? (coeff term))
    ;         term-list
    ;         (cons term term-list)))
     
    ; (define (first-term term-list) (car term-list))

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
        (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
    (put 'negate '(polynomial)
        (lambda (x) (tag (negate-poly x))))
    (put 'div '(polynomial polynomial)
        (lambda (p1 p2) (tag (div-poly p1 p2))))  
        
    'done
)

(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms)
)

(define (=zero? x) (apply-generic '=zero? x))
(define (negate x) (apply-generic 'negate x))

(install-polynomial-package)

(define x (make-polynomial 'x '(1 2 4 0 5)))
(define y (make-polynomial 'x '(1 0 0 0 -1)))
(define z (make-polynomial 'x '(1 0 -1)))