(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\even-fibs_2.2.3.scm")
(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap1\\prime.scm")

(define (enum-interval x y)
    (if(> x y)
        nil
        (cons x (enum-interval (+ 1 x) y))
    )
)

(define (flatmap proc seq)
    (accumulate
        append 
        nil
        (map proc seq)
    )
)

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter 
            prime-sum?
            (unique-pairs n)
        )
    )
)

(define (permutation s)
    (if (null? s)
        (list nil)
        (flatmap 
            (lambda (x)
                (map 
                    (lambda (p) (cons x p))
                    (permutation (remove x s))))
            s
        )
    )
)

(define (remove item sequence)
    (filter
        (lambda (x) (not (= x item)))
        sequence
    )
)

(define (unique-pairs n)
    (flatmap (lambda (i)
                (map (lambda (j) (list i j))
                    (enum-interval 1 (- i 1))))
            (enum-interval 1 n)
    )
)

;(display (prime-sum-pairs 9))