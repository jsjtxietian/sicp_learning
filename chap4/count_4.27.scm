(load "leval_4.2.scm")

(define count 0)

(define (id x) (set! count (+ count 1)) x)

(define w (id (id 10)))

