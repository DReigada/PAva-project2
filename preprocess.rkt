#lang racket
(provide add-active-token def-active-token); process-string)
(provide get-token-list find-token); TODO remove this

(define *token-list* '())

(define (add-active-token token function)
    (set! *token-list* (cons (cons token function) *token-list*)))

; (define (process-string str)
;     )

(define (find-token str token-list)
    (and
        (not (null? token-list))
        (let ([token-pair (car token-list)])
            (if (starts-with? str (car token-pair))
                (cdr token-pair)
                (find-token str (cdr token-list))
            ))))

(define (starts-with? str expr)
    (let   ([expr-length (string-length expr)]
            [str-length (string-length str)])
        (and 
            (>= str-length expr-length)
            (string=?
                (substring str 0 (string-length expr))
                expr))))


(define-syntax-rule (def-active-token token parameters body)
    (add-active-token token (lambda parameters body)))


(define (get-token-list) *token-list*)
