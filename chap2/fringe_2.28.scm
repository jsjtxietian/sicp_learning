(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
    (cond ((null? tree)                         ; 空树
            '())
        ((not (pair? tree))                   ; 叶子
            (list tree))
        (else
            (append (fringe (car tree))         ; 累积左子树所有元素
                    (fringe (cadr tree)))) ; 累积右子树所有元素
            )     
)    

(display (fringe x))