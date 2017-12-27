(load "let_4.6.scm")

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
        ((letrec? exp)
            (eval (letrec->let exp) env))
        ((begin? exp) 
            (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
            (APPLY (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
            (error "Unknown expression type -- EVAL" exp))))

(define (letrec? expr) 
    (tagged-list? expr 'letrec))

(define (letrec-inits expr) 
    (cadr expr))

(define (letrec-body expr) 
    (cddr expr)) 
        
(define (declare-variables expr) 
    (map 
        (lambda (x) (list (car x) ''*unassigned*)) 
        (letrec-inits expr))) 

(define (set-variables expr) 
    (map 
        (lambda (x) (list 'set! (car x) (cadr x))) 
        (letrec-inits expr))) 

(define (letrec->let expr) 
    (list 'let (declare-variables expr)  
        (make-begin 
            (append (set-variables expr) (letrec-body expr))))) 



(define the-global-environment (setup-environment))
(driver-loop)

;test
(define (f x)
    (letrec 
        ((even? 
            (lambda (n) 
                (if (= n 0)
                    true
                    (odd? (- n 1)))))
        (odd? 
            (lambda (n) 
                (if (= n 0)
                    false
                    (even? (- n 1))))))
    (odd? x)))

