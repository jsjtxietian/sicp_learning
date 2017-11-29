(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random high))))

(define (random-number-pairs low1 high1 low2 high2) 
    (cons-stream 
        (cons (random-in-range low1 high1) (random-in-range low2 high2)) 
        (random-number-pairs low1 high1 low2 high2))) 


(define (estimate-integral p x1 x2 y1 y2) 
    (let ((area (* (- x2 x1) (- y2 y1))) 
            (randoms (random-number-pairs x1 x2 y1 y2))) 
        (scale-stream (monte-carlo (stream-map p randoms) 0 0) area)))
        
(define (sum-of-square x y) 
    (+ (* x x) (* y y))) 
(define f 
    (lambda (x) 
        (not (> (sum-of-square (- (car x) 1) (- (cdr x) 1)) 1)))) 
(define pi-stream (estimate-integral f 0 2 0 2)) 
