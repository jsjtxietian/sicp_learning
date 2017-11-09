
(define (make-queue)
    (let ((front-ptr '())
            (rear-ptr '()))

        (define (empty-queue?) (null? front-ptr))
        
        (define (front-queue) 
            (if (empty-queue?)
                (error "FRONT called with an empty queue" )
                (car front-ptr)))

        (define (delete-queue!)
            (if (empty-queue?)
                (error "DELETE called with an empty queue" )
                (set! front-ptr (cdr front-ptr))))

        (define (insert-queue! item)
            (let ((new-pair (cons item '())))
                (if (empty-queue?) 
                    (begin
                        (set! front-ptr  new-pair)
                        (set! rear-ptr  new-pair))
                    (begin
                        (set-cdr! rear-ptr new-pair)
                        (set! rear-ptr new-pair))
                )
            )
        )

        (define (print-queue)
            (display front-ptr)
        )

        (define (dispatch m)
            (cond 
                ((eq? m 'empty-queue?) empty-queue?)
                ((eq? m 'insert-queue!) insert-queue!)
                ((eq? m 'delete-queue!) delete-queue!)
                ((eq? m 'front-queue) front-queue)
                ((eq? m 'print-queue) print-queue)
                (else 
                    (error "Undefined instruction! -- DISPTACH" m))    
            )
        )

        dispatch
    )
)

(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (front-queue queue) ((queue 'front-queue)))
(define (print-queue queue) ((queue 'print-queue)))

(define z (make-queue))


