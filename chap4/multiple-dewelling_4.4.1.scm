(define (distinct? items)
    (cond 
        ((null? items) 
            #t)
        ((null? (cdr items))
            #t)
        ((member? (car items) (cdr items))
            #f)
        (else 
            (distinct? (cdr items)))))

(define (member? item x)  
    (cond 
        ((null? x) '())  
        ((equal? item (car x)) 
            x)  
        (else 
            (memq item (cdr x)))))  

(define (flatmap proc li) 
    (if (null? li) 
        '() 
        (let ((result (proc (car li))) 
              (rest (flatmap proc (cdr li)))) 
          (if (pair? result) 
              (append result rest) 
              (cons result rest))))) 
   
(define (permutations lists) 
    (if (null? lists) 
        '(()) 
        (flatmap 
            (lambda (x)  
                (map 
                    (lambda (y) (cons x y))  
                    (permutations (cdr lists)))) 
            (car lists)))) 
   
(define (restrictions l) 
    (apply 
      (lambda (baker cooper fletcher miller smith) 
        (and (> miller cooper) 
          (not (= (abs (- smith fletcher)) 1)) 
          (not (= (abs (- fletcher cooper)) 1)) 
          (distinct? (list baker cooper fletcher miller smith)))) 
      l)) 
   
(define (mutiple-dwelling) 
    (let ((baker '(1 2 3 4)) 
          (cooper '(2 3 4 5)) 
          (fletcher '(2 3 4)) 
          (miller '(3 4 5)) 
          (smith '(1 2 3 4 5))) 
      (filter 
        restrictions 
        (permutations (list baker cooper fletcher miller smith))))) 

; (define baker '(1 2 3 4))
; (define cooper '(2 3 4 5))
; (define fletcher '(2 3 4))
; (define miller '(3 4 5))
; (define smith '(1 2 3 4 5))