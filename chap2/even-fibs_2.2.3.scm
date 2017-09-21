(define square (lambda (x) (* x x)))

(define (fib n)
    (fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q n)
    (cond ((= n 0)
            b)
        ((even? n)
            (fib-iter a 
                    b
                    (+ (square p) (square q))     ; 计算 p'
                    (+ (* 2 p q) (square q))      ; 计算 q'
                    (/ n 2)))
        (else
            (fib-iter (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- n 1)
            )
        )
    )
)

; (define (sum-odd-squares tree)
;     (cond
;         ((null? tree) 0)
;         ((not (pair? tree)) 
;             (if (odd? tree)
;                 (square tree)
;                 0
;             )
;         )
;         (else
;             (+
;                 (sum-odd-squares (car tree))
;                 (sum-odd-squares (cdr tree))
;             )
;         )    
;     )
; )

; (define (even-fibs n)
;     (define (next k)
;         (if (> k n)
;             nil
;             ( let ((f (fib k)))
;                 (if (even? f) 
;                     (cons f (next (+ k 1)))
;                     (next (+ k 1))    
;                 )
;             )
;         )
;     )

;     (next 0)
; )



(define (filter predicate sequence)
    (cond 
        ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))    
    )
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (enumerate_interval low high)
    ( if (> low high)
        nil
        (cons low (enumerate_interval (+ 1 low ) high))
    )
)

(define (enumerate_tree tree)
    ( cond 
        ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate_tree (car tree)) (enumerate_tree (cdr tree))))
    )
)

(define (sum-odd-squares tree)
    (accumulate +
                0
                (map square 
                    (filter odd? (enumerate_tree tree))                    
                )
    )
)

(define (even-fibs n)
    (accumulate cons
                nil
                (filter 
                    even? 
                    (map fib (enumerate_interval 0 n))    
                )
                
    )
)

(define (list-fib-squares n)
    (accumulate cons
                nil
                (map
                    square
                    (map
                        fib
                        (enumerate_interval 0 n)
                    )
                )
    )
)

(define (product-of-squares-of-odd-elements sequence)
    (accumulate * 
                1
                (map
                    square 
                    (filter odd? sequence)                 
                )
    )
)

;(display (product-of-squares-of-odd-elements (list 1 2 3 4 5)))