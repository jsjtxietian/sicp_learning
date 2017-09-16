;方法一
; (define (cons x y)
;     (define (dispatch m)
;         (cond
;             ((= m 0) x)
;             ((= m 1) y)
;             (else (error "Argument not 0 or 1 --  CONS" m))
;         )
;     )
;     dispatch
;     ; (lambda (m)
;     ;     (cond 
;     ;         ((= m 0) x)
;     ;         ((= m 1) y)
;     ;         (else (error "Argument not 0 or 1 --  CONS" m))
;     ;     )
;     ; )
; )

; (define (car z) (z 0))
; (define (cdr z) (z 1))


;方法二
; (define (cons x y)
;     (lambda (m)
;         (m x y)
;     )
; )

; (define (car z)
;     (z (lambda (p q) p))
; )

; (define (cdr z)
;     (z (lambda (p q) q))
; )


; (display (car (cons 5 2)))

;方法三

(define (cons x y)
    ( * (expt 2 x) (expt 3 y))    
)

(define (car x)
    (if (= (remainder x 2) 0)
        (+ 1 (car (/ x 2)))
        0
    )
)

(define (cdr x)
    (if (= (remainder x 3) 0)
        (+ 1 (cdr (/ x 3)))
        0
    )
)

(display (cdr (cons 3 4)))