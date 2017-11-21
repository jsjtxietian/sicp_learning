(load "stream_3.5.scm")

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

(define one-to-ten (stream-enumerate-interval 1 10))
(display-stream (stream-map + one-to-ten one-to-ten))
