(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\tree-list_2.63.scm")
(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\list-tree_2.64.scm")
(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\ordered-union-set_2.62.scm")


(define (intersection-tree tree another)
    (list->tree
        (intersection-set 
            (tree->list-2 tree)
            (tree->list-2 another)
        )
    )
)

(define (union-tree tree another)
    (list->tree
        (union-set 
            (tree->list-2 tree)
            (tree->list-2 another)
        )
    )
)