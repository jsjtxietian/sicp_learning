(load "eval_4.1.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp)
            (eval (let->combination exp) env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (APPLY (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (let? exp)
    (tagged-list? exp 'let))
    
(define (let-vars expr) (map car (cadr expr))) 
(define (let-inits expr) (map cadr (cadr expr))) 
(define (let-body expr) (cddr expr)) 
    
(define (let->combination expr) 
    (cons (make-lambda (let-vars expr) (let-body expr)) 
        (let-inits expr))) 


;(define the-global-environment (setup-environment))
; (driver-loop)












           