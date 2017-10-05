(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\Huffman_2.3.4.scm")

(define sample-tree
    (make-code-tree
        (make-leaf 'A 4)
        (make-code-tree 
            (make-leaf 'B 2)
            (make-code-tree (make-leaf 'C 1)
                            (make-leaf 'D 1))))
)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(display (decode sample-message sample-tree ))
;(a d a b b c a)