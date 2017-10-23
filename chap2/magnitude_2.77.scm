(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\complex_2.4.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
        (lambda (x) (tag x)))
    (put 'exp '(scheme-number scheme-number)
        (lambda (x y) (tag (expt x y))))
    (put 'raise '(scheme-number) 
        (lambda (x)
            (make-rational x 1)))
    (put 'equal '(scheme-number scheme-number)
        (lambda (x y) (= x y)))
    (put 'sine '(scheme-number) 
        (lambda (x) (tag (sin x)))) 
    (put 'cosine '(scheme-number) 
        (lambda (x) (tag (cos x))))   

    (put '=zero? '(scheme-number)
        (lambda (x) 
            (= x 0)))
    (put 'negate '(scheme-number)
        (lambda (x) (tag (- x))))


    'done
)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-rational-package)
    ;; internal procedures
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                (* (denom x) (numer y))))

    ;; interface to rest of the system
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
    (put 'raise '(rational) 
        (lambda (x)
            (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
    (put 'equal '(rational rational)
        (lambda (x y) 
            (and
                (= (numer x) (numer y))
                (= (denom x) (denom y)))))
    (put 'project '(rational)
        (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))  
    (put 'sine '(rational) 
        (lambda (x) (sin (/ (numer x) (denom x)))))  
    (put 'cosine '(rational) 
        (lambda (x) (cos (/ (numer x) (denom x)))))       
            
    (put '=zero? '(rational)
        (lambda (x) 
                (= (numer x) 0)))
    (put 'negate '(rational)
        (lambda (x) 
                (tag (make-rat (- (numer x)) (denom x)))))
    

    'done
)


(define (make-rational n d) ((get 'make 'rational) n d))

(define (install-complex-package)
    ;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))
    ;; internal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                            (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                            (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                            (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                            (- (angle z1) (angle z2))))
    

    ;; interface to rest of the system
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'equal '(complex complex)
        (lambda (x y) 
            (and 
                (= (real-part x) (real-part y))
                (= (imag-part x) (imag-part y)))))
    (put 'project '(complex)
        (lambda (x) 
            (make-rational (real-part x) 1)))
    (put '=zero? '(complex)
        (lambda (x) 
            (and 
                (= (real-part x) 0)
                (= (imag-part x) 0))))
    (put 'negate '(complex)
        (lambda (x) 
            (make-from-real-imag (- (real-part x)) (- (imag-part x)))))

    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle     '(complex) angle)
    
    'done
)

(define (make-complex-from-real-imag x y) 
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a) 
    ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)
(install-rational-package)
(install-scheme-number-package)


