(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\vect_2.46.scm")

; (define (make-frame origin edge1 edge2)
;     (list origin edge1 edge2)
; )

; (define (origin-frame f)
;     (car f)
; )

; (define (edge1-frame f)
;     (cadr f)
; )

; (define (edge2-frame f)
;     (caddr f)
; )

(define (make-frame origin edge1 edge2)
    (cons origin
        (cons edge1 edge2))
)

(define (origin-frame f)
    (car f)
)

(define (edge1-frame f)
    (cadr f)
)

(define (edge2-frame f)
    (cddr f)
)

(define (frame-coord-map frame)
    (lambda (v)
        (add-vect 
            (origin-frame frame)
            (add-vect 
                (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))
            )
        )
    )
)