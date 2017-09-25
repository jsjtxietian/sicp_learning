(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\unique-pairs_2.40.scm")

(define (unique-triples n)
    (flatmap (lambda (i)
                (map (lambda (j)                   ; cons 起 i 元素和二元组 j ，组成三元组
                        (cons i j))
                    (unique-pairs (- i 1))))      ; 生成不大于 i 的所有相异整数二元组
            (enum-interval 1 n))               ; 生成 1 至 n 的所有整数，作为 i 
)            

(define (equal-to? sum triple)
    (= 
        sum 
        (accumulate + 0 triple)
    )
)

(define (remove n triple)
    (filter 
        (lambda (current-triple) (equal-to? n current-triple))
        triple
    )
)

(display (unique-triples 5))