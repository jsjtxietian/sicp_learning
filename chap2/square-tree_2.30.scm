(define (square x) (* x x))
; (define (scale-tree tree factor)
;     (cond
;         ((null? tree) nil)
;         ((not (pair? tree)) (* factor tree))
;         (else
;             (cons (scale-tree (car tree) factor ) (scale-tree (cdr tree) factor))
;         )
;     )
; )

; (define (scale-tree tree factor)
;     (map 
;         (lambda (subtree) 
;             (if (pair? subtree)
;                 (scale-tree subtree factor)
;                 (* subtree factor)
;             )    
;         )
        
;         tree
;     )
; )


; (define (square-tree tree)
;     (cond
;         ((null? tree) nil)
;         ((not (pair? tree)) (square tree))
;         (else
;             (cons (square-tree (car tree)) (square-tree (cdr tree)))
;         )    
;     )
; )

; (define (square-tree tree)
;     (map
;         (lambda (subtree) 
;             (if (pair? subtree)
;                 (square-tree subtree)
;                 (square subtree)
;             )    
;         )  
        
;         tree
;     )
; )

(define (square-tree tree) (tree-map square tree))

; (define (tree-map proc tree)
;     (cond
;         ((null? tree) '())
;         ((not (pair? tree)) (proc tree))
;         (else
;             (cons (tree-map proc (car tree)) (tree-map proc (cdr tree)))
;         )    
;     )
; )

(define (tree-map proc tree)
    (map
        (lambda (subtree) 
            (if (pair? subtree)
                (tree-map proc subtree)
                (proc subtree)
            )    
        )  
        
        tree
    )    

)




(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(display (square-tree x))