; ;(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap1\\prime.scm")

; ;;构造&&选择
; (define (cons-stream a b)
;     (cons a (delay b)))

; (define (force delayed-object)
;     (delayed-object))
; (define the-empty-stream '())
; (define stream-null? null?)

(define (stream-car s)
    (car s))
(define (stream-cdr s)
    (force (cdr s)))

(define (delay f)
    (memo-proc (lambda () f)))


(define (memo-proc proc)
    (let ((already-run? #f) (result false))
        (lambda () 
            (if (not already-run?)
                (begin 
                    (set! result (proc))
                    (set! already-run? #t)
                    result)
            result))))

;;构造流
(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                    (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin 
            (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))

(define (display-line x)
    (newline)
    (display x))

(define (stream-filter pred stream)
    (cond 
        ((stream-null? stream)
            the-empty-stream)
        ((pred (stream-car stream))
            (cons-stream (stream-car stream)
                        (stream-filter pred
                                (stream-cdr stream))))
        (else 
            (stream-filter pred (stream-cdr stream)))))

;;测试            
(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))

; (display 
;     (stream-car 
;         (stream-cdr 
;             (stream-filter prime? 
;                     (stream-enumerate-interval 10000 1000000)))))

;;infinite streams


(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens 
    (stream-filter 
        (lambda (x) 
            (not (divisible? x 7)))
        integers))

;(stream-ref no-sevens 100)

;;斐波那契
(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;;素数筛
(define (sieve stream)
    (cons-stream
        (stream-car stream)
        (sieve 
            (stream-filter 
                (lambda (x) 
                    (not (divisible? x (stream-car stream))))
                (stream-cdr stream)))))       
(define primes (sieve (integers-starting-from 2)))

;(stream-ref primes 50)

;;Defining streams implicitly
(define ones (cons-stream 1 ones))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc 
                (map (lambda (s) 
                        (stream-car s))
                    argstreams))
            (apply stream-map 
                (cons proc 
                    (map (lambda (s) 
                            (stream-cdr s)) 
                        argstreams))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define integers 
    (cons-stream 
        1 
        (add-streams ones integers)))

(define fibs 
    (cons-stream 0
        (cons-stream 1
            (add-streams (stream-cdr fibs)
                fibs))))

(define (scale-stream stream factor)
    (stream-map 
        (lambda (x) 
            (* x factor))
        stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define square (lambda (x) (* x x)))

(define primes
    (cons-stream 
        2
        (stream-filter 
            prime? 
            (integers-starting-from 3))))

(define (prime? n)
    (define (iter ps)
        (cond 
            ((> (square (stream-car ps)) n)
                #t)
            ((divisible? n (stream-car ps))
                #f)
            (else 
                (iter (stream-cdr ps)))))
    (iter primes))

;;3.5.3
(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
            (stream-map (lambda (guess) 
                            (sqrt-improve guess x))
                        guesses)))
    guesses)

;;pai
(define (pi-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map - (pi-summands (+ n 2)))))

; (define pi-stream
;     (scale-stream (partial-sums (pi-summands 1)) 4))


(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
            (s1 (stream-ref s 1))
            (s2 (stream-ref s 2)))
        (cons-stream (- s2 (/ (square (- s2 s1))
                            (+ s0 (* -2 s1) s2)))
                    (euler-transform (stream-cdr s)))))

(define (make-tableau transform  s)
    (cons-stream s
        (make-tableau transform
            (transform s))))

(define (accelerated-sequence transform s)
    (stream-map stream-cdr 
        (make-tableau transform s)))

