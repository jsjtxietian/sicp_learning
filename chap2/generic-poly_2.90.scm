(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\poly_2.5.3.scm")

; 稠密多项式
(define (install-dense-package)
   
    (define (adjoin-term term term-list)
        (cond 
            ((=zero? (coeff term)) term-list)
            ((= (order term) (length term-list)) (cons (coeff term) term-list))
            (else
                (adjoin-term term (cons 0 term-list)))))
    (define (the-empty-termlist) '())
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list)
        (null? term-list))
        
    (define (first-term term-list)
        (list (- (length term-list) 1) (car term-list)))
    (define (make-term-for-dense order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))
    (define (add-terms L1 L2)
        (cond 
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else 
            (let ((t1 (length L1))
                    (t2 (length L2)))
                (let ((diff (- t1 t2)))
                (cond
                    ((= 0 diff) 
                    (cons (add (car L1) (car L2))
                            (add-terms (cdr L1) (cdr L2))))
                    ((> 0 diff)  ; t2 阶数高
                    (cons (car L2)
                            (add-terms L1 (cdr L2))))
                    (else
                    (cons (car L1)
                            (add-terms (cdr L1) L2)))))))))
    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (let ((rests (rest-terms L1)))
                (add-terms (mul-term-by-all-terms (first-term L1) L2)
                    (mul-terms rests L2)))))

    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term L)))
            (adjoin-term
                (make-term-for-dense (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
                (mul-term-by-all-terms t1 (rest-terms L))))))

    (define (negate-dense p)
        (map 
            (lambda (x) 
                (negate x)) 
            p))    
        

    ; 外部接口
    (define (tag t) (cons 'dense t))
    (put 'adjoin-term '(dense)
        (lambda (term term-list) (tag (adjoin-term term term-list))))
    (put 'add-terms '(dense dense)
        (lambda (L1 L2) (tag (add-terms L1 L2))))
    (put 'mul-terms '(dense dense)
        (lambda (L1 L2) (tag (mul-terms L1 L2))))
    (put 'negate 'dense
        (lambda (p) (tag (negate-dense p))))
'done
)

; 稀疏多项式
(define (install-sparse-package)
;内部实现
    (define (adjoin-term term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (the-empty-termlist) '())
    (define (first-term term-list)
        (car term-list))
    (define (rest-terms term-list)
        (cdr term-list))
    (define (empty-termlist? term-list)
        (null? term-list))
    (define (make-term-for-sparse order coeff)
        (list order coeff))
    (define (order term)
        (car term))
    (define (coeff term)
        (cadr term))

    (define (negate-sparse p)
        (map 
            (lambda (term) 
                (make-term-for-sparse (order term) (negate (coeff term)))) 
            p))

    (define (add-terms L1 L2)
        (cond 
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else 
            (let ((t1 (first-term L1))
                    (t2 (first-term L2)))
                (cond 
                ((> (order t1) (order t2)) 
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                ((< (order t1) (order t2)) 
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                (else
                    (adjoin-term
                        (make-term-for-sparse (order t1) (add (coeff t1) (coeff t2)))  
                        (add-terms (rest-terms L1) (rest-terms L2)))))))))

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
                (make-term-for-sparse (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
                (mul-term-by-all-terms t1 (rest-terms L))))))

    ; 外部接口
    (define (tag t) (attach-tag 'sparse t))

    (put 'adjoin-term 'sparse
        (lambda (term term-list) (tag (adjoin-term term term-list))))
    (put 'add-terms '(sparse sparse)
        (lambda (L1 L2) (tag (add-terms L1 L2))))
    (put 'mul-terms '(sparse sparse)
        (lambda (L1 L2) (tag (mul-terms L1 L2))))
    (put 'negate 'sparse 
        (lambda (p) (tag (negate-sparse p))))
'done
)


(install-dense-package)
(install-sparse-package)


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

    (define (add-terms L1 L2) (apply-generic 'add-terms L1 L2))
    (define (mul-terms L1 L2) (apply-generic 'mul-terms L1 L2))

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
    
    (define (adjoin-term term term-list)
        (let ((proc (get 'adjoin-term (type-tag term-list))))
            (proc term (contents term-list))
            (error "error types -- term-list " term-list)))
    (define (negate-poly p)
        (attach-tag (variable p) ((get 'negate (type-tag (term-list p))) (contents (term-list p)))))   

    (define (tag p) (attach-tag 'polynomial p))

    (put 'add '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) 
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial-dense 
        (lambda (var terms) (tag (make-poly var (attach-tag 'dense terms)))))
    (put 'make 'polynomial-sparse 
        (lambda (var terms) (tag (make-poly var (attach-tag 'sparse terms)))))
    (put 'negate '(polynomial)
        (lambda (p) (tag (negate-poly p)))) 
    (put 'sub '(polynomial polynomial)
        (lambda (x y) (tag (add-poly x (negate-poly y)))))
            
    'done
)

(install-polynomial-package)

(define (make-polynomial-dense var terms)
    ((get 'make 'polynomial-dense) var terms)
)

(define (make-polynomial-sparse var terms)
    ((get 'make 'polynomial-sparse) var terms)
)

(define x (make-polynomial-dense 'x '(1 2 3 0 5)))
(define z (make-polynomial-dense 'x '(1 2 3 1 5 6)))

(define y (make-polynomial-sparse 'x '((10 1) (5 2) (1 1))))

