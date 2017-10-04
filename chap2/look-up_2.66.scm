(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\union-tree_2.65.scm")

; (define (look-up given-key set-of-record)
;     (cond
;         ((null? set-of-record)  #f)
;         ((equal? given-key (key (car set-of-record))) (car set-of-record))
;         (else
;             (look-up given-key (cdr set-of-record)))
;     )
; )

(define (look-up given-key tree)
    (if (null? tree)
        #f
        (let
            ((current (key (entry tree))))
            (cond
                ((= given-key current)  current)
                ((< given-key current)  (look-up given-key (left-branch tree)))
                ((> given-key current)  (look-up given-key (right-branch tree)))
            )
        )
    )
)