(define zero
    (lambda (f) (lambda (x) x))    
)

(define (add-1 n)
    (lambda (f) 
        (f ((n x) x))
    )
)

(define one
    (lambda (f) (lambda (x) (f x)))
)

(define two
    (lambda (f)
        (lambda (x)
            (f (f x)))
    )
)

(define (plus first second)
    (lambda (f)
        (lambda (x)
            ((first f) ((second f) x)))
    )
)

(define (mult first second)
    (lambda (f)
        (lambda (x)
            ((first (second f)) x))
    )
)

(define (power m n)
  (n m)
)


;;; 以下是验证

(define inc (lambda (x) (+ 1 x)))

(define three
    (lambda (f) (lambda (x) (f (f (f x))))))

(define five
    (lambda (f)
        (lambda (x) ((three f) ((two f) x)))))
    
(display "((two inc) 0)")
(display ((two inc) 0))
( newline)

(display "((three inc) 0)")
(display ((three inc) 0))
( newline)

(display "((five inc) 0)")
(display ((five inc) 0))
( newline)

(display "((three (two inc)) 0)")
(display ((three (two inc)) 0))
( newline)


(display "(((power two three) inc) 0)")
(display (((power two three) inc) 0))
(newline)


(display "(((five (three two)) inc) 0)")
(display (((five (three two)) inc) 0))
( newline)

(display "(((zero (five (three two))) inc) 0)")
(display (((zero (five (three two))) inc) 0))
( newline)
