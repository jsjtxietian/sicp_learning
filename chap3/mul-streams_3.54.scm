(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define factorials 
    (cons-stream 1
        (mul-streams 
            factorials
            (stream-cdr integers ))))

(stream-ref factorials 4)

