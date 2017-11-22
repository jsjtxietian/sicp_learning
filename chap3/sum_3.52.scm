(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define sum 0)

(define (accum x )
    (set! sum (+ sum x))
    sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x)
    (= (remainder x 5) 0))
    seq))

(stream-ref y 7);16
(display-stream z)

