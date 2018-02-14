(load "leval_4.2.scm")

(define (delay-it-with-memo exp env)
	(list 'thunk-memo exp env))

(define (thunk-memo? obj)
	(tagged-list? obj 'thunk-memo))

(define (force-it obj)
	(cond 
		((thunk? obj)
			(actual-value (thunk-exp obj) (thunk-env obj)))
	  	((thunk-memo? obj)
			  (let 
				  ((result (actual-value
						  (thunk-exp obj)
						  (thunk-env obj))))
			 (set-car! obj 'evaluated-thunk)
			 (set-car! (cdr obj) result)  ; replace exp with its value
			 (set-cdr! (cdr obj) '())     ; forget unneeded env
			 result))
		((evaluated-thunk? obj)
			(thunk-value obj))
	  	(else obj)))

(define (APPLY procedure arguments env) 
	(cond 
		((primitive-procedure? procedure) 
			(apply-primitive-procedure 
				procedure 
				(list-of-arg-values arguments env))) 
		((compound-procedure? procedure) 
			(eval-compound-procedure procedure arguments env)) 
		(else 
			(error "Unknown procedure type -- APPLY" procedure)))) 
 
(define (eval-compound-procedure procedure arguments env) 
	(define (iter-args formal-args actual-args) 
		(if (null? formal-args) 
			'() 
			(cons 
				(let ((this-arg (car formal-args))) 
					(cond 
						((and (pair? this-arg) 
							 	(pair? (cdr this-arg)) 
							 	(eq? (cadr this-arg) 'lazy-memo)) 
							(delay-it-with-memo (car actual-args) env))
						((and (pair? this-arg) 
								(pair? (cdr this-arg)) 
								(eq? (cadr this-arg) 'lazy)) 
						   	(delay-it (car actual-args) env))
						 ;force the argument if it is not lazy.  
						(else 
							(actual-value (car actual-args) env)))) 
				(iter-args (cdr formal-args) (cdr actual-args))))) 
 
	(define (procedure-arg-names parameters) 
		(map (lambda (x) (if (pair? x) (car x) x)) parameters)) 
 
	(eval-sequence 
		(procedure-body procedure) 
		(extend-environment 
			(procedure-arg-names (procedure-parameters procedure)) 
			(iter-args  
				(procedure-parameters procedure) 
				arguments) 
			(procedure-environment procedure)))) 

(define the-global-environment (setup-environment))
(driver-loop)

(define (id x) 
	(set! count (+ count 1)) x) 

(define count 0) 

(define (square x) (* x x)) 

(square (id 10))

(define (square (x lazy)) (* x x)) 

(define (square (x lazy-memo)) (* x x)) 

