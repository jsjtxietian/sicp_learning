(load "leval_4.2.scm")



(define (p1 x)
    (set! x (cons x '(2)))
    x)

(define (p2 x)
    (define (p e)
        e
        x)
    (p (set! x (cons x '(2)))))