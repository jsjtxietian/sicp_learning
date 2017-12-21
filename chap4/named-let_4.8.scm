(load "let_4.6.scm")

(define (named-let? expr) 
    (and 
        (let? expr) 
        (symbol? (cadr expr)))) 

(define (named-let-func-name expr) 
    (cadr expr)) 

(define (named-let-func-body expr) 
    (cadddr expr)) 

(define (named-let-func-parameters expr) 
    (map car (caddr expr))) 

(define (named-let-func-inits expr) 
    (map cadr (caddr expr))) 

(define (named-let->func expr) 
    (list 'define  
        (cons (named-let-func-name expr) (named-let-func-parameters expr)) 
        (named-let-func-body expr))) 

(define (let->combination expr) 
    (if (named-let? expr) 
        (sequence->exp 
            (list (named-let->func expr) 
                    (cons (named-let-func-name expr) (named-let-func-inits expr)))) 
        (cons 
            (make-lambda (let-vars expr) 
                (list (let-body expr))) 
            (let-inits expr)))) 


(define (fib n)
    (let 
        fib-iter
        ((a 1) (b 0) (count n))
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1)))))

(define the-global-environment (setup-environment))
(driver-loop)