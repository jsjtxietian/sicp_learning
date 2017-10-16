(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\magnitude_2.77.scm")

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args))) 
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                          (if (eq? type1 type2)
                            (error "No method for these types" (list op type-tags))
                            (let ((t1->t2 (get-coercion type1 type2))
                                    (t2->t1 (get-coercion type2 type1)))
                                (cond
                                    (t1->t2 
                                        (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1
                                        (apply-generic op a1 (t2->t1 a2)))
                                    (else
                                        (error "No method for these types" (list op type-tags))
                                    )   
                                )
                            )
                          )
                    )
                    (error "No method for these types" (list op type-tags))
                )
            )
        )
    )
)

; (define (exp x y) (apply-generic 'exp x y))

; (define (scheme-number->scheme-number n) n)
; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
; (define (complex->complex z) z)
; (put-coercion 'complex 'complex complex->complex)
