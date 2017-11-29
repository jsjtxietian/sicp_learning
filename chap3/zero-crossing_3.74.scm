(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")


; (define (make-zero-crossings input-stream last-value)
;     (cons-stream
;         (sign-change-detector (stream-car input-stream) last-value)
;         (make-zero-crossings 
;             (stream-cdr input-stream)
;             (stream-car input-stream))))

; (define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
    (stream-map 
        sign-change-detector
        sense-data
        (cons-stream 
            0
            sense-data)))

;;3.75
(define (make-zero-crossings input-stream last-value last-avpt) 
    (let ((avpt (/ (+ (stream-car input-stream) last-value) 2))) 
            (cons-stream 
                (sign-change-detetor avpt last-avpt) 
                (make-zero-crossings 
                    (stream-cdr input-stream) 
                    (stream-car input-stream) 
                    avpt)))) 

;;3.76
(define (smooth s)
    (stream-map 
        (lambda 
            (x y)
            (/ (+ x y) 2))
        (cons-stream 0 s)
        s))

(define (make-zero-crosssings input-stream smooth) 
    (let ((after-smooth (smooth input-stream))) 
        (stream-map sign-change-detector after-smooth (cons-stream 0 after-smooth)))) 

;;another 3.76
(define (smooth input-stream) 
    (stream-map (lambda(x y)(/ (+ x y) 2)) input-stream (stream-cdr input-stream))) 
   
(define (zero-crossings input-stream) 
    (stream-map sign-change-detector input-stream (stream-cdr input-stream))) 
   
(define (smoothed-zero-crossing sense-data) 
    (zero-crossings (smooth sense-data))) 



