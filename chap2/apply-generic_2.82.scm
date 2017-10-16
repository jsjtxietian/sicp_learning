(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap2\\apply-generic_2.81.scm")

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args))) 
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (
                    ;todo
                )
            )
        )
    )
)

(define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (scheme-number->scheme-number n) n)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(define (complex->complex z) z)
(put-coercion 'complex 'complex complex->complex)
