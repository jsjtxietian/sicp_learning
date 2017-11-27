(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define (all-pairs s t)
    (cons-stream 
            (list (stream-car s) (stream-car t))
            (interleave
                    (interleave
                            (stream-map (lambda (x) (list (stream-car s) x))
                                                    (stream-cdr t))
                            (all-pairs (stream-cdr s) (stream-cdr t)))
                    (stream-map (lambda (x) (list x (stream-car t)))
                                            (stream-cdr s)))))

