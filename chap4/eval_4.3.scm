;(load "eval_4.1.scm")
(load "table_help.scm")

(define apply-in-underlying-scheme apply)
(define true #t)
(define false #f)

; (define (eval exp env)
;   (cond ((self-evaluating? exp) exp)
;         ((variable? exp) (lookup-variable-value exp env))
;         ((quoted? exp) (text-of-quotation exp))
;         ((assignment? exp) (eval-assignment exp env))
;         ((definition? exp) (eval-definition exp env))
;         ((if? exp) (eval-if exp env))
;         ((lambda? exp)
;          (make-procedure (lambda-parameters exp)
;                          (lambda-body exp)
;                          env))
;         ((begin? exp) 
;          (eval-sequence (begin-actions exp) env))
;         ((cond? exp) (eval (cond->if exp) env))
;         ((application? exp)
;          (APPLY (eval (operator exp) env)
;                 (list-of-values (operands exp) env)))
;         (else
;          (error "Unknown expression type -- EVAL" exp))))

(define (eval exp env)
    (if (self-evaluating? exp)
        exp
        (let ((eval-proc (get 'eval (expression-type exp))))
            (eval-proc 
                (expression-content exp)
                    env))))

(define (expression-type exp)
    (cond 
        ((variable? exp) '(variable))
        ((quoted? exp) '(quoted))
        ((assignment? exp) '(assignment))
        ((definition? exp) '(definition))
        ((if? exp) '(if))
        ((lambda? exp) '(lambda))
        ((begin? exp) '(begin))
        ((cond? exp) '(cond))
        ((application? exp) '(application))
        (else 
            (error "Unknown expression type -- EVAL" exp))))
        
(define (expression-content exp) exp)

; (define (install-cond)
;     (define (cond-clauses exp) (cdr exp))
;     (define (cond-else-clause? clause)
;         (eq? (cond-predicate clause) 'else))
;     (define (cond-predicate clause) (car clause))
;     (define (cond-actions clause) (cdr clause))
;     (define (cond->if exp)
;         (expand-clauses (cond-clauses exp)))
;     (define (sequence->exp seq)
;         (cond ((null? seq) seq)
;               ((last-exp? seq) (first-exp seq))
;               (else (make-begin seq))))
;     (define (make-begin seq) (cons 'begin seq))
;     (define (expand-clauses clauses)
;         (if (null? clauses)
;             'false                          ; no else clause
;             (let ((first (car clauses))
;                     (rest (cdr clauses)))
;                 (if (cond-else-clause? first)
;                     (if (null? rest)
;                         (sequence->exp (cond-actions first))
;                         (error "ELSE clause isn't last -- COND->IF"
;                             clauses))
;                     (make-if (cond-predicate first)
;                             (sequence->exp (cond-actions first))
;                             (expand-clauses rest))))))

;     (put 'eval '(cond)
;         (eval (cond->if exp) env))
;     'done)


(define (install-application)
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))
    (define (APPLY procedure arguments)
        (cond 
            ((primitive-procedure? procedure)
                (apply-primitive-procedure procedure arguments))
            ((compound-procedure? procedure)
                (eval-sequence
                    (procedure-body procedure)
                    (extend-environment
                        (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))
            (else
                (error
                    "Unknown procedure type -- APPLY" procedure))))        
    (define (apply-primitive-procedure proc args)
        (apply-in-underlying-scheme
            (primitive-implementation proc) args))
    (define (primitive-implementation proc) (cadr proc))

    (put 'eval 'application 
        (lambda (exp env) 
            (APPLY (eval (operator exp) env)
                (list-of-values (operands exp) env))))
    'done)


(define (install-lambda)
    (define (make-procedure parameters body env)
        (list 'procedure parameters body env))
    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))

    (put 'eval '(lambda)
        (lambda (exp env) 
            (make-procedure 
                (lambda-parameters exp)
                (lambda-body exp)
                env)))
    'done)

(define (install-begin)
    (define (begin-actions exp) (cdr exp))

    (put 'eval '(begin)
        (lambda (exp env) (eval-sequence (begin-actions exp) env)))
    'done)

(define (install-if)
    (define (eval-if exp env)
        (if (true? (eval (if-predicate exp) env))
            (eval (if-consequent exp) env)
            (eval (if-alternative exp) env)))

    (define (if-predicate exp) (cadr exp))

    (define (if-consequent exp) (caddr exp))
    
    (define (if-alternative exp)
        (if (not (null? (cdddr exp)))
            (cadddr exp)
            'false))
    
    (put 'eval '(if) 
        (lambda (exp env) (eval-if exp env)))
    'done)

(define (install-definition)
    (define (definition-variable exp)
        (if (symbol? (cadr exp))
            (cadr exp)
            (caadr exp)))

    (define (definition-value exp)
        (if (symbol? (cadr exp))
            (caddr exp)
            (make-lambda (cdadr exp)
                        (cddr exp))))

    (define (eval-definition exp env)
        (define-variable! (definition-variable exp)
                            (eval (definition-value exp) env)
                            env)
        'ok)
    (put 'eval '(definition)
        (lambda (exp env) (eval-definition exp env)))
    'done)

(define (install-variable)
    (define (lookup-variable-value var env)
        (define (env-loop env)
            (define (scan vars vals)
            (cond ((null? vars)
                    (env-loop (enclosing-environment env)))
                    ((eq? var (car vars))
                    (car vals))
                    (else (scan (cdr vars) (cdr vals)))))
            (if (eq? env the-empty-environment)
                (error "Unbound variable" var)
                (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                        (frame-values frame)))))
        (env-loop env))

    (put 'eval '(variable)
        (lambda (exp env)
            (lookup-variable-value exp env)))
    
    'done)

(define (install-quoted)
    (define (text-of-quotation exp) (cadr exp))
    (put 'eval '(quoted)
        (lambda (exp env) (text-of-quotation exp)))
    'done)

(define (install-assignment)
    (define (set-variable-value! var val env)
        (define (env-loop env)
            (define (scan vars vals)
            (cond ((null? vars)
                    (env-loop (enclosing-environment env)))
                    ((eq? var (car vars))
                    (set-car! vals val))
                    (else (scan (cdr vars) (cdr vals)))))
            (if (eq? env the-empty-environment)
                (error "Unbound variable -- SET!" var)
                (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                        (frame-values frame)))))
        (env-loop env))

    (define (eval-assignment exp env)
        (set-variable-value! (assignment-variable exp)
                            (eval (assignment-value exp) env)
                            env))

    (put 'eval '(assignment)
        (lambda (exp env) (eval-assignment exp env)))
    'ok)


;;judge
(define (if? exp) (tagged-list? exp 'if))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (quoted? exp)
    (tagged-list? exp 'quote))
(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))
(define (variable? exp) (symbol? exp))

(define (assignment? exp)
    (tagged-list? exp 'set!))
(define (definition? exp)
    (tagged-list? exp 'define))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (begin? exp) (tagged-list? exp 'begin))
(define (cond? exp) (tagged-list? exp 'cond))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;other
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
        (cond ((null? vars)
                (add-binding-to-frame! var val frame))
                ((eq? var (car vars))
                (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
            (frame-values frame))))

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
            (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;;env
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))
  
(define the-empty-environment '())

(define (make-frame variables values)
(cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
(set-car! frame (cons var (car frame)))
(set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
(if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
        (error "Too many arguments supplied" vars vals)
        (error "Too few arguments supplied" vars vals))))

  
(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
        (cond ((null? vars)
                (env-loop (enclosing-environment env)))
                ((eq? var (car vars))
                (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
            (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                    (frame-values frame)))))
    (env-loop env))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define primitive-procedures
    (list (list 'car car)
            (list 'cdr cdr)
            (list 'cons cons)
            (list 'null? null?)
            (list '+ +)
            (list '- -)
            (list '/ /)
            (list '* *)
    ;;      more primitives
            ))
    
(define (primitive-procedure-names)
    (map car
            primitive-procedures))
            

(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
            primitive-procedures))

 
;;interact        
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))

(define (announce-output string)
    (newline) (display string) (newline))

(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>))
        (display object)))

;;ready
(define the-global-environment (setup-environment))

(define (install-all)
    (install-application)
    (install-assignment)
    (install-begin)
    ;(install-cond)
    (install-definition)
    (install-if)
    (install-lambda)
    (install-quoted)
    (install-variable))
;(driver-loop)