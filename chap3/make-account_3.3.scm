(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\withdraw_3.1.1.scm")

(define (make-account balance password)

    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                    balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)

    (define (display-wrong-password-message useless-arg)                ; 新增
        (display "Incorrect password"))                                 ;
    

    (define (dispatch pw m)
        (if (eq? pw password)
            (cond 
                ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else
                    (error "Unknown request -- MAKE-ACCCOUNT" m)))
            display-wrong-password-message
        )
    )

    dispatch
)

(define acc (make-account 100 'fuck))