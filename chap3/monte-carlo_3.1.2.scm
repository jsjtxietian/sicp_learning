(define rand-init 233)


;;使用rand
(define rand
    (let ((x rand-init))
        (lambda () 
            (set! x (rand-update x))
            x)))

(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test) (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond 
            ((= trials-remaining 0) 
                (/ trials-passed trials))
            ((experiment)
                (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else
                (iter (- trials-remaining 1) trials-passed))))
                
    (iter trials 0)                
)

;;直接用rand-update
(define (estimate-pi trials)
    (sqrt (/ 6 (random-gcd-test trials rand-init))))

(define (random-gcd-test trials initial-x)
    (define (iter trials-remaining trials-passed x)
        (let ((x1 (rand-update x)))
            (let ((x2 (rand-update x1)))
                (cond
                    ((= trials-remaining 0)
                        (/ trials-passed trials))
                    ((= (gcd x1 x2) 1)
                        (iter (- trials-remaining 1)
                            (+ trials-passed 1)
                            x2))
                    (else
                        (iter (- trials-remaining 1)
                            trials-passed
                            x2))
                )
            )
        )
    )

    (iter trials 0 initial-x)
)



    

;;from support
(define (rand-update x)
    (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))