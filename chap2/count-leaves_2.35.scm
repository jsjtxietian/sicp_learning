(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\even-fibs_2.2.3.scm")

(define x (cons (list 1 2) (list 3 4 5)))

; (define (count-leaves tree)
;     (accumulate
;         (lambda (x y) 
;             (+ 1 y))
;         0
;         (enum tree)
;     )
; )

; (define (enum tree)
;     (cond
;         ((null? tree) nil)
;         ((not (pair? tree)) tree)
;         (else (append (car tree) (cdr tree)))
;     )
; )

(define (count-leaves tree)
    (accumulate
        +
        0
        (map 
            (lambda (subtree ) 
                (if (not (pair? subtree))
                    1
                    (count-leaves subtree)
                )
            )
            tree
        )
    )
)


(display (count-leaves x))