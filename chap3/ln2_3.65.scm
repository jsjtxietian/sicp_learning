(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")
(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\partial-sums_3.55.scm")

(define (ln2-stream n)
    (cons-stream (/ 1.0 n)
             (stream-map - (ln2-stream (+ n 1)))))

(define ln2
    (partial-sums (ln2-stream 1)))

(euler-transform ln2)
(accelerated-sequence euler-transform ln2)