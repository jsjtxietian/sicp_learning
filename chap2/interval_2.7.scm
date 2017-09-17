(define (print-interval name i) 
    (newline) 
    (display name) 
    (display ": [") 
    (display (lower-bound i)) 
    (display ",") 
    (display (upper-bound i)) 
    (display "]")
) 

(define (make-interval x y) 
    (cons x y)
)

(define (add-interval x y)
    (make-interval
        (+ (lower-bound x) (lower-bound y)) 
        (+ (upper-bound x) (upper-bound y))
    )
)

(define (mult-interval x y)
    (let 
        (
            (p1 (* (lower-bound x) (lower-bound y)))
            (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y)))
        )  
        
        (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
    )
)

(define (div-interval x y)
    (if (>= 0 (* (upper-bound y) (lower-bound y)))
        (error "Division error (interval spans 0)" y) 
        (mult-interval 
            x 
            (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))
        )
    )
)

(define (lower-bound x)
    (min (car x) (cdr x))
)

(define (upper-bound x)
    (max (car x) (cdr x))
)

(define (sub-interval x y) 
    (make-interval (- (lower-bound x) (upper-bound y)) 
                   (- (upper-bound x) (lower-bound y))
    )
) 

; (define (mul-interval x y) 
;     (define (endpoint-sign i)  
;       (cond ((and (>= (upper-bound i) 0) 
;                   (>= (lower-bound i) 0)) 
;              1) 
;             ((and (< (upper-bound i) 0) 
;                   (< (lower-bound i) 0)) 
;              -1) 
;             (else 0))) 

;     (let 
;         ((es-x (endpoint-sign x)) 
;           (es-y (endpoint-sign y)) 
;           (x-up (upper-bound x)) 
;           (x-lo (lower-bound x)) 
;           (y-up (upper-bound y)) 
;           (y-lo (lower-bound y))) 

;       (if (and (= es-x 0) (= es-y 0)) 
;         ; Take care of the exceptional condition where we have to test 
;         (make-interval (min (* x-lo y-up) (* x-up y-lo)) 
;                        (max (* x-lo y-lo) (* x-up y-up))) 

;         ; Otherwise, select which value goes in which "slot". I'm not sure 
;         ; whether there is an intuitive way to explain *why* these 
;         ; selections work. 
;         (let ((a1 (if (and (<= es-y 0) (<= (- es-y es-x) 0)) x-up x-lo)) 
;               (a2 (if (and (<= es-x 0) (<= (- es-x es-y) 0)) y-up y-lo)) 
;               (b1 (if (and (<= es-y 0) (<= (+ es-y es-x) 0)) x-lo x-up)) 
;               (b2 (if (and (<= es-x 0) (<= (+ es-x es-y) 0)) y-lo y-up))) 
;           (make-interval (* a1 a2) (* b1 b2)))))
; ) 

; (define i (make-interval 2 7)) 
; (define j (make-interval 8 3)) 
; (define span-0 (make-interval -1 1)) 
; (print-interval "i/j" (div-interval i j)) 
; (print-interval "i/span-0" (div-interval i span-0))

; (define (make-center-width c w)
;     (make-interval (+ c w) (- c w))
; )

; (define (center i)
;     (/ (+ (lower-bound i) (upper-bound i)) 2)
; )

; (define (width i)
;     (/ (- (upper-bound i) (lower-bound i)) 2)
; )

(define (make-center-percent c p)
    (
        let
        ((width (* c (/ p 100.0))))
        (make-interval (+ c width) (- c width))
    )
)

(define (percent x)
    (
        let
        (
            (a (lower-bound x))
            (b (upper-bound x))
        )
        (* (/ (- b a) (+ a b)) 100.0)
    )
)

(define i (make-center-percent 1 10))
(display (percent i))




