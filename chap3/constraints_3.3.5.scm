
;;using
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-concerter C F)

(define (celsius-fahrenheit-concerter c f)
    (let ((u (make-connector))
            (v (make-connector))
            (w (make-connector))
            (x (make-connector))
            (y (make-connector)))
        (multiplier c w u)
        (multiplier v x u)
        (adder v y f)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)
        'ok))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

;;Implementing
(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond 
            ((and (has-value? a1) (has-value? a2))
                (set-value! sum 
                            (+ (get-value a1) (get-value a2))
                            me))
            ((and (has-value? a1) (has-value? sum))
                (set-value! a2 
                            (- (get-value sum) (get-value a1))
                            me))
            ((and (has-value? a2) (has-value? sum))
                (set-value! a1
                            (- (get-value sum) (get-value a2))
                            me))))
    (define (process-forget-value)
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))
    (define (me request)
        (cond 
            ((eq? request 'I-have-a-value)
                (process-new-value))
            ((eq? request 'I-lost-my-value)
                (process-forget-value))
            (else 
                (error "Unkown request -- ADDER" request))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
me)


(define (multiplier m1 m2 product)
    (define (process-new-value)
        (cond 
            ((or (and (has-value? m1) (= (get-value m1) 0))
                    (and (has-value? m2) (= (get-value m2) 0)))
                (set-value! product 0 me))
            ((and (has-value? m1) (has-value? m2))
                (set-value! product 
                            (* (get-value m1) (get-value m2))
                            me))
            ((and (has-value? m1) (has-value? product))
                (set-value! m2 
                            (/ (get-value product) (get-value m1))
                            me))
            ((and (has-value? m2) (has-value? product))
                (set-value! m1
                            (/ (get-value product) (get-value m2))
                            me))))
    (define (process-forget-value)
        (forget-value! product me)
        (forget-value! m1 me)
        (forget-value! m2 me)
        (process-new-value))
    (define (me request)
        (cond 
            ((eq? request 'I-have-a-value)
                (process-new-value))
            ((eq? request 'I-lost-my-value)
                (process-forget-value))
            (else 
                (error "Unkown request -- MULTIPLIER" request))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)


(define (inform-about-value constraint)
    (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))
(define (constant value connector)
    (define (me request)
        (error "Unknown request -- CONSTANT" request))
    (connect connector me)
    (set-value! connector value me)
    me)

(define (probe name connector)
    (define (print-probe value)
        (newline)
        (display name)
        (display " = ")
        (display value)
        (newline))
    (define (process-new-value)
        (print-probe (get-value connector)))
    (define (process-forget-value)
        (print-probe "?"))
    (define (me request)
        (cond 
            ((eq? request 'I-have-a-value)
                (process-new-value))
            ((eq? request 'I-lost-my-value)
                (process-forget-value))
            (else  
                (error "Unknown request -- PROBE" request))))
    (connect connector me)
    me)

    