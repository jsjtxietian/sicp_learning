(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\ordered-set_2.3.3.scm")

(define (adjoin-set x set)
    (if (null? set)
        (list x)
        (let 
            ((current (car set))    (remain (cdr set)))
            (cond
                ((= x current) set)
                ((< x current) 
                    (cons x set))
                ((> x current)
                    (cons current (adjoin-set x remain)))
            )
        )
    )
)

