(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\magnitude_2.77.scm")

(define (attach-tag type-tag contents)
    (if (number? contents) 
        contents
        (cons type-tag contents)        
    )
)

(define (type-tag datum) 
    (if (pair? datum)
        (car datum)
        (if (number? datum)
            'scheme-number
            (error "Bad tagged datum -- TYPE-TAG " datum)            
        )
    )
)

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (if (number? datum)
            datum
            (error "Bad tagged datum -- TYPE-TAG " datum)                        
        )
    )
)
