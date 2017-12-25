;;去看sicp 理解Y组合子

(define x 
    ((lambda (n) 
        ((lambda (fact) 
            (fact fact n))
            (lambda (ft k) 
                (if (= k 1)
                    1
                    (* k (ft ft (- k 1)))))))
        10))