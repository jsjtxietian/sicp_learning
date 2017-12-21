(load "eval_4.1.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ; ((and? exp) (eval-and (and-exp exp) env))
        ; ((or? exp) (eval-or (or-exp exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
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

(define (eval-or exps env)
  (let ((ans (eval (first-exp exps) env)))
    (if ans 
      #t
      (cond 
        ((last-exp? exps) #f)
        (else 
          (eval-or (rest-exps exps) env))))))

(define (and->if exps)
  (expand-and-operands (and-exp exps)))
(define (or->if exps)
  (expand-or-operands (or-exp exps)))

(define (expand-and-operands exps)
  (if (null? exps)  
    'true
    (make-if 
      (first-exp exps)
      (expand-and-operands (rest-exps exps))
      'false)))
      
(define (expand-or-operands exps)
  (if (null? exps)  
    'false
    (make-if 
      (first-exp exps)
      'true
      (expand-or-operands (rest-exps exps)))))
    
(define the-global-environment (setup-environment))
; (driver-loop)
