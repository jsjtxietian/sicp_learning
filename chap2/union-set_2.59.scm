(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\set_2.3.3.scm")

(define (union-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) 
            (union-set (cdr set1) set2))
        (else 
            (cons (car set1) (union-set (cdr set1) set2)))
    )
)

; (define (union-set set1 set2)
;     (iter (append set1 set2) '())
; )

; (define (iter input result)
;     (if (null? input)
;         (reverse result)
;         (let ((current-element (car input))
;                 (remain-element (cdr input)))
;             (if (element-of-set? current-element result)
;                 (iter remain-element result)
;                 (iter remain-element
;                     (cons current-element result))
;             )
;         )
;     )
; )