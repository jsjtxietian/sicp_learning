 (load "D://sicp//chap2//segment_2.2.scm")

 (define (perimeter-rectangle r)
    (let ((length (length-of-rectangle r))
        (width (width-of-rectangle r)))
        
        (* 2 
            (+ length width)
        )
    )
)

(define (area-rectangle r)
    (* (length-of-rectangle r)
    (width-of-rectangle r))
)

(define (make-rectangle length width)
    (cons length width)
)

(define (length-rectangle r)
    (car r)
)

(define (width-rectangle r)
    (cdr r)
)

(define (length-of-rectangle r)
    (let ((length (length-rectangle r)))
        (let(
                (start (start-segment length))
                (end (end-segment length))
            )
            (- (x-point end)
               (x-point start)
            )
        )
    )
)

(define (width-of-rectangle r)
    (let ((width (width-rectangle r)))
        (let ((start (start-segment width))
            (end (end-segment width)))
            (- (y-point end)
            (y-point start))
        )
    )
)

(define l (make-segment (make-point 1 2)
    (make-point 4 2))
)

(define w (make-segment (make-point 1 2)
    (make-point 1 4))
)

(define r (make-rectangle l w))

(display (area-rectangle r))
