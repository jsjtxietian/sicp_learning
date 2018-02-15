;;;正确性有待验证

(define (an-integer-between a b) 
    (require (<= a b)) 
    (amb a (an-integer-between (+ a 1) b))) 
 
;;check if (car solution) is compatible with any of (cdr solution) 
(define (safe? solution)  
    (let ((p (car solution))) 
        (define (conflict? q i) 
            (or 
                (= p q) 
                (= p (+ q i)) 
                (= p (- q i)))) 
        (define (check rest i) 
            (cond  
                ((null? rest) #t) 
                ((conflict? (car rest) i) #f) 
                (else (check (cdr rest) (inc i))))) 
        (check (cdr solution) 1))) 
 
(define (queens n) 
    (define (iter solution n-left) 
        (if (= n-left 0) 
            (begin 
                (display solution) 
                (newline)) 
            (begin 
                (let ((x-solution (cons (an-integer-between 1 n) solution))) 
                    (require (safe? x-solution)) 
                    (iter x-solution (- n-left 1)))))) 
    (iter '() n)) 
 
(queens 8) 
