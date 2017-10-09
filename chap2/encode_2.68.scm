(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\decode-sample_2.67.scm")

(define (memq item x)
    (cond
        ((null? (symbols x)) false)
        ((eq? item (car (symbols x))) true)
        (else (memq item (cdr (symbols x))))
    )
)

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))
    )
)

(define (encode-symbol c tree)
    (cond 
        ((leaf? tree) '())
        ((memq c (left-branch tree)) 
            (cons 0 (encode-symbol c (left-branch tree))))
        ((memq c (right-branch tree)) 
            (cons 1 (encode-symbol c (right-branch tree))))
        (else
            (error "This symbol not in tree: " c))
    )
)


;(display (encode '(A D A B B C A) sample-tree))