(load "amb_4.3.3.scm")

;;check 

(define the-global-environment (setup-environment))
(driver-loop)


(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond 
      ((null? items) 
          true)
      ((null? (cdr items))
          true)
      ((member? (car items) (cdr items))
          false)
      (else 
          (distinct? (cdr items)))))

(define (member? item x)  
  (cond 
      ((null? x) '())  
      ((equal? item (car x)) 
          x)  
      (else 
          (memq item (cdr x)))))  

(define (father-daughter) 
    (let ((Moore 'Mary) 
          (Barnacle 'Melissa) 
          (Hall (amb 'Gabrielle 'Lorna)) 
          (Downing (amb 'Gabrielle 'Lorna 'Rosalind)) 
          (Parker (amb 'Lorna 'Rosalind))) 
      (require (cond ((eq? Hall 'Gabrielle) (eq? 'Rosalind Parker)) 
                     ((eq? Downing 'Gabrielle) (eq? 'Melissa Parker)) 
                     (else false))) 
      (require (distinct? (list Hall Downing Parker))) 
      (list (list 'Barnacle Barnacle) 
            (list 'Moore Moore) 
            (list 'Hall Hall) 
            (list 'Downing Downing) 
            (list 'Parker Parker)))) 