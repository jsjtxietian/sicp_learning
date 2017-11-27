(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")


(define (stream-take-while pred? stream)
    (if (stream-null? stream)
        '()
        (if (pred? (stream-car stream))
            (cons-stream (stream-car stream)
                        (stream-take-while pred? (stream-cdr stream)))
            '())))

(define (stream->list s)
    (if (null? (stream-cdr s))
        (list (stream-car s))
        (cons (stream-car s) (stream->list (stream-cdr s)))))

; (stream->list
;     (stream-take-while (lambda (x)
;                             (< x 10))
;                         integers))

(define before-1-100 (stream->list
    (stream-take-while
        (lambda (pair)
            (not (equal? pair '(1 100))))
        (pairs integers integers))))
