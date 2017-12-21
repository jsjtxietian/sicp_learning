(load "eval_4.1.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((and? exp) (eval-and (and-exp exp) env))
        ((or? exp) (eval-or (or-exp exp) env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
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

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (and-exp exp) (cdr exp))
(define (or-exp exp) (cdr exp))

(define (eval-and exps env)
  (let ((ans (eval (first-exp exps) env)))
    (if ans 
      (cond 
        ((last-exp? exps)
          #t)
        (else 
          (eval-and (rest-exps exps) env)))
      #f)))




      
(define the-global-environment (setup-environment))
; (driver-loop)