(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\make-frame_2.47.scm")

(define (make-segment start end)
    (list start end)
)

(define (start-segment s)
    (car s)
)

(define (end-segment s)
    (cadr s)
)

