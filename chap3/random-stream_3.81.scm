(load "C:\\Users\\jsjtx\\Desktop\\sicp_learning\\chap3\\stream_3.5.scm")

(define (random-number-generator command-stream) 
    (define random-number 
            (cons-stream 
                rand-init 
                    (stream-map 
                        (lambda (number command)  
                            (cond 
                                ((null? command) the-empty-stream) 
                                ((eq? command 'generator) 
                                    (rand-update number)) 
                                ((and (pair? command)  (eq? (car command) 'reset)) 
                                    (cdr command)) 
                                (else  
                                    (error "bad command -- " commmand)))) 
                        random-number 
                        command-stream))) 
    random-number) 




