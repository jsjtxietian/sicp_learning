;;; 1-list-of-values-from-left-to-right.scm

(define (list-of-values exps env)
    (if (not-operands? exps)
        '()
        (let ((first-value (eval (first-operand exps) env)))
            (cons first-value
                  (list-of-values (rest-operands exps) env)))))

;;; 1-list-of-values-from-right-to-left.scm

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (let ((rest-values (list-of-values (rest-operands exps) env)))
            (cons (eval (first-operand exps) env)
                  rest-values))))