(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\make-account_3.3.scm")

(define (make-joint acc pw1 pw2)
    (lambda (give-pw mode) 
        (if (eq? give-pw pw2) 
            (acc pw1 mode)
            (error "WRONG PW --MAKE-JOINT" pw2)
        )  
    )
)

