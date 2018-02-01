(load "eval_4.1.scm")

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((unless? exp) (eval (unless->if exp) env)) 
        ((lambda? exp)
            (make-procedure (lambda-parameters exp)
                (lambda-body exp)
                env))
        ((begin? exp) 
            (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
            (APPLY (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
            (error "Unknown expression type -- EVAL" exp))))

  
;; unless expression is very similar to if expression. 
(define (unless? expr) (tagged-list? expr 'unless)) 
(define (unless-predicate expr) (cadr expr)) 
(define (unless-consequence expr) 
    (if (not (null? (cdddr expr))) 
        (cadddr expr) 
        'false)) 
(define (unless-alternative expr) (caddr expr)) 

(define (unless->if expr) 
    (make-if (unless-predicate expr) (unless-consequence expr) (unless-alternative expr))) 


(define the-global-environment (setup-environment))
(driver-loop)

