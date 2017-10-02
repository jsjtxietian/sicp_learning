(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\exponentiation_2.56.scm")

(define (make-sum a1 . a2)
    (if (single-operand? a2)
        (let ((a2 (car a2)))
            (cond ((=number? a1 0)
                    a2)
                ((=number? a2 0)
                    a1)
                ((and (number? a1) (number? a2))
                    (+ a1 a2))
                (else
                    (list '+ a1 a2))
            )
        )
        (cons '+ (cons a1 a2)))
)

(define (sum? x)
    (and (pair? x)
        (eq? (car x) '+)
    )
)

(define (addend s)  (cadr s))

(define (augend s)
    (let ((tail-operand (cddr s)))
        (if (single-operand? tail-operand)
            (car tail-operand)
            (apply make-sum tail-operand)
        )
    )
)

(define (make-product m1 . m2)
    (if (single-operand? m2)
        (let ((m2 (car m2)))
            (cond ((or (=number? m1 0) (=number? m2 0))
                    0)
                ((=number? m1 1)
                    m2)
                ((=number? m2 1)
                    m1)
                ((and (number? m1) (number? m2))
                    (* m1 m2))
                (else
                    (list '* m1 m2))
            )
        )
        (cons '* (cons m1 m2))
    )
)

(define (product? x)
    (and 
        (pair? x)
        (eq? (car x) '*)
    )
)

(define (multiplier p)  (cadr p))

(define (multiplicand p)
    (let ((tail-operand (cddr p)))
        (if (single-operand? tail-operand)
            (car tail-operand)
            (apply make-product tail-operand))
    )
)

(define (single-operand? exp)
    (if (null? (cdr exp))
        #t
        #f
    )
)

(display  (deriv '(* x y (+ x 3)) 'x))