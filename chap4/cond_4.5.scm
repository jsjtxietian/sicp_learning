(load "eval_4.1.scm")

(define (special-cond? clause) 
    (eq? (cadr clause) '=>)) 
   
(define (cond-actions clause) 
    (if (special-cond? clause) 
        (cons (caddr clause) (cond-predicate clause)) 
        (cdr clause))) 


(define the-global-environment (setup-environment))
; (driver-loop)