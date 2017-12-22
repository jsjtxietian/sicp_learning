;(load "eval_4.1.scm")

;;bug!!!
(define (make-frame vars vals)
    (map cons vars vals))

(define (frame-variables f)
    (map car f))

(define (frame-values f)
    (map cdr f))


;;; (define (add-binding-to-frame! var val frame)
;;;     (set! frame (cons (cons var val) frame)))



