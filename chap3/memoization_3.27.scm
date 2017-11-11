(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\one-dimensional-table_3.3.3.scm")

(define memo-fib
    (memoize 
        (lambda (n)
            (cond 
                ((= n 0) 0)
                ((= n 1) 1)
                (else 
                    (+ (memo-fib (- n 1))
                        (memo-fib (- n 2))))))))

(define (memoize f)
    (let ((table (make-table)))
        (lambda (x)
            (let ((previously-computed-result (lookup x table)))
                (or previously-computed-result ;利用or的性质，找到了直接返回 否则进行插入操作
                    (let ((result (f x)))
                        (insert! x result table)
                        result
                    )
                )
            )
        )
    )
)

(define (fib n)
    (cond 
        ((= n 0) 0)
        ((= n 1) 1)
        (else
            (+ (fib (- n 1))
                (fib (- n 2))))))
