(define (delay exp)
 (lambda () exp))

(extend-syntax (cons-stream)
  [(cons-stream x y)
   (cons x (delay y))]) 
 
