;(load "C:\\Users\\jsjtx\\Desktop\\sicp\\fixedPoint_1.3.3.scm")
(load "D:\\sicp\\fixedPoint_1.3.3.scm")

(define (square x)
    (* x x)
)

(define (deriv g)
    (lambda (x) 
        (/ (- (g (+ x dx)) (g x)) dx )
    )
)

(define dx 0.00001)

(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) x)))
    )
)

(define (newtons-method g guess)
    (fixed_point (newton-transform g) guess)
)

(define (sqrt x)
    (newtons-method (lambda (y) (- (square y) x)) 1.0)
)

(display (sqrt 3))