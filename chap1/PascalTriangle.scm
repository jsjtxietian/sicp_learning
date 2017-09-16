(define (p row col)
    (cond ((= col 1) 1)
        ((= row col) 1)
        (else
            (+ 
                (p (- row 1) (- col 1))
                (p (- row 1) col)
            )
        )
    )
)



(display (p 3 2))