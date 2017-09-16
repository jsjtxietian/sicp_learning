(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))
    )
)

(define (make-rat n d)
    (if (< d 0)
        (cons (- n) (- d))
        (cons n d)
    )
)

(define (numer x)
    (car x)
)

(define (denom x)
    (cdr x)
)

(define (print-rat x)
    ;(newline)
    (display (numer x))
    (display "/")
    (display (denom x))
)

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x))
              )

              (* (denom x) 
                 (denom y)
              )
    )
)

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x))
              )

              (* (denom x) 
                 (denom y)
              )
    )
)

(define (mul-rat x y)
    (make-rat 
        (* (numer x) (numer y))
        (* (denom x) (denom y))    
    )
)

(define (div-rat x y)
    (make-rat 
        (* (numer x) (denom y))
        (* (denom x) (numer y))    
    )
)

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x)) 
    )
)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)


(define one-half (make-rat 2 4))
(define one-third (make-rat 12 36))

(print-rat (add-rat one-half one-third))





