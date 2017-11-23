(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

;;a)
(define (div-streams s1 s2)
    (stream-map / s1 s2))

(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define (integrate-series a)
    (mul-streams a                                  
                (div-streams ones integers)))     

;;b)
(define exp-series
    (cons-stream 1
        (integrate-series exp-series)))

(define consine-series
    (cons-stream 1
        (integrate-series 
            (scale-stream 
                sine-series
                -1)))) 
        

(define sine-series
    (cons-stream 0
        (integrate-series 
            consine-series)))

(define (mul-series s1 s2)
    (cons-stream 
        (* (stream-car s1) 
            (stream-car s2))
        (add-streams 
            (scale-stream (stream-cdr s2) (stream-car s1))
            (mul-series (stream-cdr s1) s2))))

(define (reciprocal-series s)
    (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (reciprocal-series s)) -1)))

(define (div-series s1 s2)
    (let ((c (stream-car s2)))
        (if (= c 0)
            (error "constant term of s2 can't be 0!")
            (scale-stream (mul-series s1
                                    (reciprocal-series (scale-stream s2 (/ 1 c))))
                        (/ 1 c)))))
  
(define tane-series (div-series sine-series cosine-series))