(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define (avr x y)
    (/ (+ x y ) 2))

(define (sqrt-improve guess x)
    (avr guess (/ x guess)))

(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
    (let ((p (stream-car s))
            (n (stream-car (stream-cdr s))))
        (if (< (abs (- p n)) tolerance)
            n
            (stream-limit (stream-cdr s) tolerance))))

(sqrt 2 0.000001)