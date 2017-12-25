(load "eval_4.1.scm")

(define (map proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst))
            (map proc (cdr lst)))))

(define the-global-environment (setup-environment))
(driver-loop)