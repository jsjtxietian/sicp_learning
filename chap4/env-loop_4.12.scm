(load "eval_4.1.scm")

(define (env-loop env var var-not-in-frame proc) 
    (define (scan vars vals) 
        (cond ((null? vars) (var-not-in-frame env)) 
            ((eq? var (car vars)) (proc vals)) 
            (else 
            (scan (cdr vars) (cdr vals))))) 
    (if (eq? env the-empty-environment) 
        (error "Unbound variable" var) 
        (let ((frame (first-frame env))) 
            (scan (frame-variables frame) 
                (frame-values frame))))) 

(define (lookup-variable-value var env) 
    (define (var-not-in-frame env) 
        (lookup-variable-value var (enclosing-environment env))) 
    (env-loop env var var-not-in-frame car)) 

(define (set-val! val) 
    (lambda (vals) (set-car! vals val))) 

(define (set-variable-value! var val env) 
    (define (var-not-in-frame env) 
        (set-variable-value! var val (enclosing-environment env))) 
    (env-loop env var var-not-in-frame (set-val! val))) 

(define (define-variable! var val env) 
    (define (var-not-in-frame env) 
        (add-binding-to-frame! var val (first-frame env))) 
    (env-loop env var var-not-in-frame (set-val! val))) 