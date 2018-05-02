#lang racket
(provide add-active-token def-active-token); process-string)
(provide get-token-list); TODO remove this

(define token-list '())

(define (add-active-token token function)
    (set! token-list (cons (cons token function) token-list)))

(define-syntax-rule (def-active-token token parameters body)
    (add-active-token token (lambda parameters body)))

(define (get-token-list) token-list)