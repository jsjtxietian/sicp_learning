(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\append_3.12.scm")


;;有bug list('a 'a)认为是环。。。
(define (check l)
    (let ((set '()))
        (define (inner x)
            (cond 
                ((null? x) #t)
                ((memq (car x) set) #f)
                (else 
                    (begin
                    (set! set (cons (car x) set))
                    (inner (cdr x))))
            )
        )
        (inner l)
    )
)

;;暴力改表就没问题
(define (loop? lst)
    (let ((identity (cons '() '())))
        (define (iter remain-list)
            (cond ((null? remain-list)
                    #f)
                ((eq? identity (car remain-list))
                    #t)
                (else
                    (set-car! remain-list identity)
                    (iter (cdr remain-list)))))
        (iter lst)
    )
)

(define z (make-cycle (list 'a 'b 'c)))