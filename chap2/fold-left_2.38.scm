(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\even-fibs_2.2.3.scm")

(define fold-right accumulate)

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))
        )
    )
    
    (iter initial sequence)
)

(define x (list 1 2 3))

; (display (fold-left list nil x))
; (newline)
; (display (fold-right list nil x))

; (display (fold-left / 1 x))
; (newline)
; (display (fold-right / 1 x))