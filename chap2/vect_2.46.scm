(define (make-vect a b)
    (list a b)
)

(define (xcor-vect vec)
    (car vec)
)

(define (ycor-vect vec)
    (cadr vec)
)

(define (add-vect a b)
    (make-vect 
        (+ (xcor-vect a) (xcor-vect b))
        (+ (ycor-vect b) (ycor-vect b))    
    )
)

(define (sub-vect a b)
    (make-vect 
        (- (xcor-vect a) (xcor-vect b))
        (- (ycor-vect b) (ycor-vect b))    
    )
)

(define (scale-vect s vec)
    (make-vect 
        (* (xcor-vect vec) s)
        (* (ycor-vect vec) s)    
    )
)
