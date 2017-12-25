(load "let_4.6.scm")

;;a)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
        ((null? vars)
             (env-loop (enclosing-environment env)))
        ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                (error "variable is unsigned! -- LOOK" var)
                (car vals)))
        (else 
            (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;b
(define (scan-out-defines body) 
  (define (name-unassigned defines) 
    (map 
      (lambda (x) (list (definition-variable x) ''*unassigned*)) 
      defines)) 
  (define (set-values defines) 
    (map (lambda (x)  
        (list 'set! (definition-variable x) (definition-value x)))  
        defines)) 
  (define (defines->let exprs defines not-defines) 
    (cond 
      ((null? exprs)  
        (if (null? defines) 
          body 
          (list 
            (list 'let 
                (name-unassigned defines)  
                (make-begin 
                  (append (set-values defines)  
                  (reverse not-defines))))))) 
      ((definition? (car exprs)) 
        (defines->let 
          (cdr exprs) 
          (cons 
            (car exprs) 
            defines) 
          not-defines)) 
      (else 
        (defines->let 
          (cdr exprs) 
          defines 
          (cons 
            (car exprs)
            not-defines))))) 
  (defines->let body '() '())) 

;c

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define the-global-environment (setup-environment))
(driver-loop)

; (let ((a 1))
; 	(define (f x)
; 		(define b (+ a x))
; 		(define a 5)
; 		(+ a b))
; 	(f 10))