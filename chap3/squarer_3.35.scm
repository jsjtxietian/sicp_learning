(load "constraints_3.3.5.scm")

(define (square x) (* x x))

(define (squarer a b)
    (define (process-new-value)
        (if (has-value? b)
            (if (< (get-value b) 0)
                (error "square less than 0 -- SQUARE" (get-value b))
                (set-value! a (sqrt (get-value b)) me))
            (if (has-value? a)
                (set-value! b (square (get-value a)) me)
                (error "neither a & b have value" a b))))
    (define (process-forget-value)
        (forget-value! a me)
        (forget-value! b me))
    (define (me request)
        (cond 
            ((eq? request 'I-have-a-value)
                (process-new-value))
            ((eq? request 'I-lost-my-value)
                (process-forget-value))
            (else 
                (error "Unkown request -- Squarer" request))))
    (connect a me)
    (connect b me)   
    me)

(define C (make-connector))
(define F (make-connector))

(probe "x" C)
(probe "x*x" F)

(squarer C F)




; (set-value! C 25 'user)
; (forget-value! C 'user)
; (set-value! F 212 'user)