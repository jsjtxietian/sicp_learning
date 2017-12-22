(load "eval_4.1.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((unbound? exp) (make-unbound (unbound-var exp) env))
        ((if? exp) (eval-if exp env))
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

(define (unbound? exp)
    (tagged-list? exp 'unbound))

(define (unbound-var exp)
    (cadr exp))

(define (make-unbound var env)
    (define (env-loop env)
        (define (scan vars vals new-vars new-vals)
            (cond ((null? vars)
                    (env-loop (enclosing-environment env)))
                    ((eq? var (car vars))
                        (set! env 
                            (cons 
                                (append new-vars (cdr vars))
                                (append new-vals (cdr vals)))))
                    (else (scan 
                            (cdr vars) 
                            (cdr vals) 
                            (cons (car vars) new-vars)
                            (cons (car vals) new-vals)))))
        (if (eq? env the-empty-environment)
            (error "Undefined variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                    (frame-values frame)
                    '()
                    '()))))
    (env-loop env))


(define the-global-environment (setup-environment))
(driver-loop)

;(define x 1)

