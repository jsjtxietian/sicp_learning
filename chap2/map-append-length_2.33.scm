(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\even-fibs_2.2.3.scm")

; (define (map p sequence)
;     (if (null? sequence)
;         '()
;         (cons (p (car sequence))
;             (map p (cdr sequence))
;         )
;     )
; )

(define (map p sequence)
    (accumulate (lambda (x y) 
                    (cons (p x) y)) 
                '()
                sequence)
)

; (define (append seq1 seq2)
;     (if (null? seq1)
;         seq2
;         (cons (car seq1) (append (cdr seq1) seq2))
;     )
; )

(define (append seq1 seq2)
    (accumulate
        cons
        seq2
        seq1
    )
)

; (define (length sequence)
;     (if (null? sequence)
;         0
;         (+ 1 (length (cdr sequence)))
;     )
; )

(define (length sequence)
    (accumulate
        (lambda (x y) 
            (+ 1 y)
        )
        0
        sequence
    )
)

(display (length (list 2 3)))