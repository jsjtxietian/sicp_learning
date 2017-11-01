
;;from support
(define (rand-update x)
    (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m))
)

(define random-init 1008611)

(define rand
    (let ((state random-init))
        (lambda (mode)
            (cond ((eq? mode 'generate)             
                    (begin 
                        (set! state (rand-update state))
                        state))
                  ((eq? mode 'reset)                ; 返回一个过程
                    (lambda (new-value)             ; 这个过程将 state 修改为新值 new-value
                        (set! state new-value)
                        state))
                 (else
                    (error "Unknow mode -- RAND" mode))))))