(load "analyze_4.1.7.scm")

(define (analyze exp)
    (cond 
        ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((quoted? exp) (analyze-quoted exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((let? exp) (analyze (let->combination exp)))
        ((lambda? exp)
            (analyze-lambda exp))
        ((begin? exp) 
            (analyze-sequence exp))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp)
            (analyze-application exp))
        (else
            (error "Unknown expression type -- ANALYZE" exp))))

            (define (let? exp)
            (tagged-list? exp 'let))
            
(define (let-vars expr) (map car (cadr expr))) 
(define (let-inits expr) (map cadr (cadr expr))) 
(define (let-body expr) (cddr expr)) 
    
(define (let->combination expr) 
    (cons (make-lambda (let-vars expr) (let-body expr)) 
        (let-inits expr))) 


(define the-global-environment (setup-environment))
(driver-loop)

