(define (smallest-division n)
    (find-division n 2)
)

(define (find-division n test-division)
    (cond 
      ((> (* test-division test-division ) n)  n)
      (( = (remainder n test-division) 0) test-division)
      (else (find-division n (next test-division)))
    )

)

(define (divides? a b)
    (= (remainder b a ) 0)
)

(define (next n)
(if (odd? n)
    (+ 2 n)
    (+ 1 n)))

(define (prime? n)
    (= n (smallest-division n))
)

;(display (prime? 32))
