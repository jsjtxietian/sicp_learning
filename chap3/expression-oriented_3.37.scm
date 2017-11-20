(load "constraints_3.3.5.scm")

(define (c+ x y)
    (let ((z (make-connector)))
        (adder x y z)
        z))

(define (cv num)
    (let ((z (make-connector)))
        (constant num z)
        z))

(define (c* x y)
    (let ((z (make-connector)))
        (multiplier x y z)
        z))

(define (c- x y)
    (let ((z (make-connector)))
        (adder x z y)
        z))

(define (c/ x y)
    (let ((z (make-connector)))
        (multiplier y z x)
        z))


(define (celsius-fahrenheit-concerter x)
    (c+ (c* (c/ (cv 9) (cv 5))
            x)
        (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-concerter C))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

