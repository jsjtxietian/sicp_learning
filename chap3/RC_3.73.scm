(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define (RC R C dt)
    (define (inner i v0)
        (add-stream
            (scale-stream i R)
            (integral (scale-stream i (/ 1 C)) v0 dt)))
    inner)

