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
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((while? exp) (eval (while->combination exp) env))
        ((application? exp)
         (APPLY (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (while? exp)
    (tagged-list? exp 'while))

(define (while-condition exp)
    (cadr exp))
(define (while-body exp)
    (caddr exp))

(define (while->combination exp)
    (sequence->exp
        (list 
            (list 'define 
                (list 'while-iter)
                (make-if 
                    (while-condition exp)
                    (sequence->exp 
                        (list (while-body exp)
                            (list 'while-iter)))
                    'true))
        (list 'while-iter))))

(define the-global-environment (setup-environment))
(driver-loop)

;(let ((x 1)) (while (< x 2) (+ x 1))