; (define (cons x y)
;     (let ((new (get-new-pair)))
;         (set-car! new x)
;         (set-cdr! new y)
;     )
; )

(define (append! x y)
    (set-cdr! (last-pair x) y)
    x
)

(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))
    )
)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define w (append! x y))

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x
)

(define z (make-cycle (list 'a 'b 'c)))

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x)
            )
        )
    )

    (loop x '())
)

 (define z (list 'a 'b 'c 'd))
 (define w (mystery z))
