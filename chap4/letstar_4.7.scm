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
        ((let*? exp)
            (eval (let*->nested-lets exp) env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (APPLY (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let*? exp)
    (tagged-list? exp 'let*))

(define (let*-body exp)
    (caddr exp))

(define (let*-inits exp)
    (cadr exp))

(define (let*->nested-lets exp)
    (let ([inits (let*-inits exp)]
            [body (let*-body exp)])
        (define (make-lets exps)
            (if (null? exps)
                body
                (list 'let (list (car exps)) (make-lets (cdr exps)) )))
        (make-lets inits)))