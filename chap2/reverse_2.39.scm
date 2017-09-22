(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\fold-left_2.38.scm")

(define (reverse sequence)
    (fold-right
        (lambda (x y) 
            (append y (list x))  
        )
        nil
        sequence
    )
)

; (define (reverse sequence)
;     (fold-left
;         (lambda (x y) (cons y x))
;         nil
;         sequence
;     )
; )

(display (reverse x))