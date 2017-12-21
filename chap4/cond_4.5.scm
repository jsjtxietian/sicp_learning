(load "eval_4.1.scm")

(define (special-cond? clause) 
    (eq? (cadr clause) '=>)) 
   
(define (cond-actions clause) 
    (if (special-cond? clause) 
        (list (caddr clause) (cond-predicate clause)) 
        (cdr clause))) 


(define the-global-environment (setup-environment))
(driver-loop)

;(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false))