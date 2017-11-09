(define (make-deque) 
    (let ((front-ptr '()) 
        (rear-ptr '())) 

    (define (front-insert-deque! item) 
        (let ((new-pair (list item '() '()))) 
            (cond ((null? front-ptr) 
                    (set! front-ptr new-pair) 
                    (set! rear-ptr new-pair)) 
                (else 
                    (set-cdr! (cdr new-pair) front-ptr) 
                    (set-car! (cdr front-ptr) new-pair) 
                    (set! front-ptr new-pair))))) 

    (define (rear-insert-deque! item) 
        (let ((new-pair (list item '() '()))) 
            (cond ((null? front-ptr) 
                    (set! front-ptr new-pair) 
                    (set! rear-ptr new-pair)) 
                (else 
                    (set-car! (cdr new-pair) rear-ptr) 
                    (set-cdr! (cdr rear-ptr) new-pair) 
                    (set! rear-ptr new-pair))))) 

    (define (front-delete-deque!) 
        (set! front-ptr (cddr front-ptr)) 
        (set-car! (cdr front-ptr) '())) 

    (define (rear-delete-deque!) 
        (set! rear-ptr (cadr rear-ptr)) 
        (set-cdr! (cdr rear-ptr) '())) 

    (define (print) 
        (define (iter x result) 
                    (if (null? (cddr x)) 
                        (append result (cons (car x) '())) 
                        (iter (cddr x) (append result (cons (car x) '()))))) 
        (iter front-ptr '())) 

    (define (dispatch m) 
        (cond 
            ((eq? m 'front-ptr) front-ptr) 
            ((eq? m 'rear-ptr) rear-ptr) 
            ((eq? m 'print) print) 
            ((eq? m 'empty-queue?) (null? front-ptr)) 
            ((eq? m 'front-delete-deque!) front-delete-deque!) 
            ((eq? m 'rear-delete-deque!) rear-delete-deque!) 
            ((eq? m 'rear-insert-deque!) rear-insert-deque!) 
            ((eq? m 'front-insert-deque!) front-insert-deque!) 
            (else 
                (error "Bad operate" m))))

    dispatch)
) 


(define mq (make-deque)) 
((mq 'rear-insert-deque!) 'a) 
((mq 'rear-insert-deque!) 'b) 
((mq 'rear-insert-deque!) 'c) 
((mq 'front-insert-deque!) 'd) 
((mq 'front-insert-deque!) 'e) 
((mq 'front-insert-deque!) 'f) 
((mq 'front-delete-deque!)) 
((mq 'rear-delete-deque!)) 
((mq 'print)) 