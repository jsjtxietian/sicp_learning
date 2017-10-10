(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\complex_2.4.scm")
(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\deriv_2.3.2.scm")

(define (attach-tag type-tag x y)
    (list type-tag x y)
)

(define (type-tag datumn)
    (car datumn)
)

(define (contents datumn)
    (cdr datumn)
)

(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
            ((get 'deriv (operator exp)) (operands exp) var)
        )
    )
)

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-sum-package)

    ;;; internal procedures 
    (define (addend s)
        (car s))

    (define (augend s)
        (cadr s))

    (define (make-sum x y)
        (cond ((=number? x 0)
                y)
              ((=number? y 0)
                x)
              ((and (number? x) (number? y))
                (+ x y))
              (else
                (attach-tag '+ x y))
        )
    )

    ;;; interface to the rest of the system
    (put 'addend '+ addend)
    (put 'augend '+ augend)
    (put 'make-sum '+ make-sum)

    (put 'deriv '+
        (lambda (exp var)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var))))
    'done
)

(define (make-sum x y)
    ((get 'make-sum '+) x y)
)

(define (addend sum)
    ((get 'addend '+) (contents sum))
)

(define (augend sum)
    ((get 'augend '+) (contents sum))
)

(define (install-product-package)

    ;;; internal procedures
    (define (multiplier p)
        (car p))

    (define (multiplicand p)
        (cadr p))

    (define (make-product x y)
        (cond ((or (=number? x 0) (=number? y 0))
                0)
              ((=number? x 1)
                y)
              ((=number? y 1)
                x)
              ((and (number? x) (number? y))
                (* x y))
              (else
                (attach-tag '* x y))))

    ;;; interface to the rest of the system
    (put 'multiplier '* multiplier)
    (put 'multiplicand '* multiplicand)
    (put 'make-product '* make-product)

    (put 'deriv '*
        (lambda (exp var)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp)))
        )
    )

    'done
)

(define (make-product x y)
    ((get 'make-product '*) x y)
)

(define (multiplier product)
    ((get 'multiplier '*) (contents product))
)

(define (multiplicand product)
    ((get 'multiplicand '*) (contents product))
)

(define (install-exponentiation-package)

    ;;; internal procedures
    (define (base exp)
        (car exp))

    (define (exponent exp)
        (cadr exp))

    (define (make-exponentiation base n)
        (cond ((= n 0)
                0)
              ((= n 1)
                base)
              (else
                (attach-tag '** base n)
              )
        )
    )

    ;;; interface to the rest of the system
    (put 'base '** base)
    (put 'exponent '** exponent)
    (put 'make-exponentiation '** make-exponentiation)

    (put 'deriv '**
        (lambda (exp var)
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
    )

    'done
)

(define (make-exponentiation base n)
    ((get 'make-exponentiation '**) base n)
)

(define (base exp)
    ((get 'base '**) (contents exp))
)

(define (exponent exp)
    ((get 'exponent '**) (contents exp))
)

