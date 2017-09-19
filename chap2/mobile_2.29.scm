(define (make-mobile left right)
    (list left right)
)

(define (make-branch length structure)
    (list length structure)
)

(define (left-branch x)
    (car x)
)

(define (right-branch x)
    (cadr x)
)

(define (branch-length x)
    (car x)
)

(define (branch-structure x)
    (cadr x)
)

(define (branch-torque x)
    (*
        (branch-length x)
        (branch-weight x)
    )
)

(define (total-weight mobile)
    (+
        (branch-weight (car mobile))
        (branch-weight (cadr mobile))
    )
)

(define (total-weight mobile)
(+ (branch-weight (left-branch mobile))         ; 计算左右两个分支的重量之和
   (branch-weight (right-branch mobile))))

(define (branch-weight branch)
    (if (hangs-another-mobile? branch)              ; 如果分支吊着另一个活动体
        (total-weight (branch-structure branch))    ; 那么这个活动体的总重量就是这个分支的重量
        (branch-structure branch))  ; 否则， 分支的 structure 部分就是分支的重量
)                

(define (hangs-another-mobile? branch)              ; 检查分支是否吊着另一个活动体
    (pair? (branch-structure branch))
)

(define (same-torque? left right)
    (= (branch-torque left)
    (branch-torque right))
)

(define (branch-balance? branch)
    (if (hangs-another-mobile? branch)              ; 如果分支上有子活动体
        (mobile-balance? (branch-structure branch))  ; 那么(递归地)检查子活动体的平衡性
        #t
    )
)   

(define (mobile-balance? mobile)
    (let ((left (left-branch mobile))
        (right (right-branch mobile)))
        (and                                        ; 必须同时满足以下三个条件，才是平衡的活动体
            (same-torque? left right)
            (branch-balance? left)
            (branch-balance? right)
        )
    )
)

(define mobile (make-mobile (make-branch 10 20)       ; 活动体的总重量为 20 + 25 = 45
    (make-branch 10 25)))

(define another-mobile (make-mobile (make-branch 10 mobile)   ; 左分支吊着另一个活动体，总重为 45
    (make-branch 10 20)))     ; 右分支的重量是 20
        
(define balance-mobile (make-mobile (make-branch 10 10)
(make-branch 10 10)))

(define unbalance-mobile (make-mobile (make-branch 0 0)
(make-branch 10 10)))


(display (mobile-balance? unbalance-mobile))
