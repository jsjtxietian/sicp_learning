(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\even-fibs_2.2.3.scm")

(define (accumulate-n op init seqs) 
    (if (null? (car seqs))
        nil
        (cons 
            (accumulate op init (car-n seqs))
            (accumulate-n op init (cdr-n seqs))    
        )
    )
)

(define (car-n seqs)
    (map car seqs)
)

(define (cdr-n seqs)
    (map cdr seqs)
)

; (define (enum-first seqs)
;     (if (null? seqs)
;         nil
;         (cons 
;             (caar seqs)
;             (enum-first (cdr seqs))
;         )
;     )
; )

; (define (enum-leave seqs)
;     (if (null? seqs)
;         nil
;         (cons 
;             (cdar seqs)
;             (enum-leave (cdr seqs))            
;         )
;     )
; )


;(define x (list (list 7 2 3 ) (list 2 3 4 ) (list 3 4 5)))

;(display (accumulate-n + 0 x))