(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\segment_2.48.scm")

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each
            (lambda (segment) 
                (draw-line
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))
                ) 
            )
            segment-list               
        )
    )
)

; (define top-left (make-vect 0.0 1.0))
; (define top-right (make-vect 1.0 1.0))
; (define bottom-left (make-vect 0.0 0.0))
; (define bottom-right (make-vect 1.0 0.0))
; (define top (make-segment top-left top-right))
; (define left (make-segment top-left bottom-left))
; (define right (make-segment top-right bottom-right))
; (define bottom (make-segment bottom-left bottom-right))

; (paint (segments->painter (list top bottom left right)))


; (let (
;         (tl (make-vect 0 1)) 
;         (tr (make-vect 1 1)) 
;         (bl (make-vect 0 0)) 
;         (br (make-vect 1 0))) 
; (paint (segments->painter (list 
;                     (make-segment bl tl) 
;                     (make-segment tl tr) 
;                     (make-segment tr br) 
;                     (make-segment br bl)))))


; ;; b 
; (segments->painter (list 
;                     (make-segment bl tr) 
;                     (make-segment br tl)))) 
 
; (let ((l (make-vect 0 0.5)) 
;     (t (make-vect 0.5 1)) 
;     (r (make-vect 1 0.5)) 
;     (b (make-vect 0.5 0))) 
; ;; c 
; (segments->painter (list 
;                     (make-segment l t) 
;                     (make-segment t r) 
;                     (make-segment r b) 
;                     (make-segment b l)))) 