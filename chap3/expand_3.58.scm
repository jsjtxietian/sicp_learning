(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define (expand num den radix)
    (cons-stream 
        (quotient (* num radix) den)
        (expand (remainder (* num  radix) den) den radix)))

;(display-stream (expand 1 7 10))