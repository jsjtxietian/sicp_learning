(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\constraints_3.3.5.scm")

(define (averager a b c)
    (let ((temp (make-connector))
            (d (make-connector)))
        (adder a b temp)
        (multiplier c d temp)
        (constant 2 d)
        'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(averager a b c)

(probe "adder 1" a)
(probe "adder 2" b)
(probe "average of a b" c)

