(load "eval_4.1.scm")

; 4.30
; (define (eval-sequence exps env)
;   (cond ((last-exp? exps) (eval (first-exp exps) env))
;         (else (actual-value (first-exp exps) env)
;               (eval-sequence (rest-exps exps) env))))

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
        ((application? exp)             ; clause from book
            (APPLY (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
            (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (APPLY procedure arguments env)
    (cond 
        ((primitive-procedure? procedure)
            (apply-primitive-procedure
                procedure
                (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
            (eval-sequence
                (procedure-body procedure)
                (extend-environment
                (procedure-parameters procedure)
                (list-of-delayed-args arguments env) ; changed
                (procedure-environment procedure))))
        (else
            (error
                "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  	(if (no-operands? exps)
      '()
      	(cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
							env))))

(define (list-of-delayed-args exps env)
  	(if (no-operands? exps)
      	'()
      	(cons (delay-it (first-operand exps) env)
            	(list-of-delayed-args (rest-operands exps)
								env))))

(define (eval-if exp env)
  	(if (true? (actual-value (if-predicate exp) env))
      	(eval (if-consequent exp) env)
      	(eval (if-alternative exp) env)))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  	(if (thunk? obj)
      	(actual-value (thunk-exp obj) (thunk-env obj))
     	obj))

;; thunks

(define (delay-it exp env)
	(list 'thunk exp env))

(define (thunk? obj)
	(tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
	(tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  (cond 
	((thunk? obj)
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


;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
	  (list 
		(list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
		))
		
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
		(prompt-for-input input-prompt)
		(let ((input (read)))
		(let ((output
			(actual-value input the-global-environment)))
				(announce-output output-prompt)
				(user-print output)))
		(driver-loop))


; (define the-global-environment (setup-environment))
; (driver-loop)