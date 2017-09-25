(define (flipped-pairs painter)
    (let
        ((painter2 (beside painter (filp-vert painter))))
        (below painter2 painter2)
    )
)

(define (right-spilt painter n)
    (if (= n 0)
        painter
        (let
            ((smaller (right-spilt painter (- n 1))))
            (beside painter (below smaller smaller))
        )
    )
)

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let 
            (
                (up (up-split painter (- n 1)))
                (right (right-spilt painter (- n 1)))
            )

            (let
                (
                    (top-left (beside up up))
                    (buttom-right (below right right))
                    (corner (corner-split painter (- n 1)))
                )
                
                (beside 
                    (below painter top-left)
                    (below buttom-right corner)
                )
            )
        )
    )
)

(define (square-limit painter n)
    (let
        ((quarter (corner-split painter n)))
        (let
            ((half (beside (flip-horiz quarter) quarter)))
            (below (flip-vert half) half)
        )
    )
)

(define (up-split painter n)
    (if (= n 0)
        painter
        (let
            ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller))
        )
    )
)

(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let
            (
                (top (beside (tl painter) (tr painter)))
                (buttom (beside (bl painter) (br painter)))
            )

            (below buttom top)
        )
    )
)

(define (flipped-pairs painter)
    (let
        ((combine4 (square-of-four identity flip-vert identity flip-vert)))
        (combine4 painter)
    )
)

(define (square-limit painter n)
    (let
        ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
        (combine4 (corner-split painter n))
    )
)


(paint (square-limit einstein 3))