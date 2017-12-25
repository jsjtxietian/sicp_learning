
(define x 
    ((lambda (n) 
        ((lambda (fact) 
            (fact fact n))
        (lambda (ft k) 
            (if (= k 1)
                1
                (* k (ft ft (- k 1)))))))
    10))

(define fib-ten
    ((lambda (k) 
        ((lambda (fib) 
            (fib fib k))
        (lambda (f n) 
            (cond 
                [(= n 0) 0]
                [(= n 1) 1]
                [else 
                    (+ (f f (- n 1))
                        (f f (- n 2)))]))))
    10))

(define (f x)
    ((lambda (even? odd?) 
        (even? even? odd? x))
    (lambda (ev? od? n) 
        (if (= n 0)
            #t
            (od? ev? od? (- n 1))))
    (lambda (ev? od? n) 
        (if (= n 0)
            #f
            (ev? ev? od? (- n 1))))))