(load "D:\\sicp\\prime.scm")

(define (next-odd n)
    (if (odd? n)
        (+ 2 n)
        (+ 1 n)
    )
)

(define (continue-primes n count)
    (cond ((= count 0)
            (display "are primes."))
          ((prime? n)
            (display n)
            (newline)
            (continue-primes (next-odd n) (- count 1)))
          (else
            (continue-primes (next-odd n) count)))
)

(define (search-for-primes n)
    (let ((start-time (runtime)))
        (continue-primes n 3)
        (- (runtime) start-time))
)

(search-for-primes 1000000)

;???