(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\deriv_2.3.2.scm")

(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) 
            (if (same-variale? exp var) 1 0))
        ((sum? exp) 
            (make-sum
                (deriv (addend exp) var)
                (deriv (augend exp) var)))
        ((product? exp)
            (make-sum
                (make-product 
                    (multipiler exp)
                    (deriv (multiplicand exp) var)    
                )
                (make-product
                    (deriv (multipiler exp) var)
                    (multiplicand exp)    
                )    
            )
        )
        ((exponentiation? exp)
            (let ((n (exponent exp))                                
                    (u (base exp)))                                   
                (make-product                                       
                    n                                               
                    (make-product                                   
                        (make-exponentiation                        
                            u                                       
                            (- n 1))                                
                        (deriv u var)
                    )
                )
            )        
        )
        (else
            (error "Unknown expression type -- DERIV" exp)
        )
    )
)

(define (make-exponentiation b e)
    (cond 
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))
    )
)

(define (base exp)  (cadr exp))

(define (exponent exp) (caddr exp))

(define (exponentiation? exp)
    (and
        (pair? exp)
        (eq? (car exp) '**)    
    )
)

;(display (deriv '(** x 0) 'x))