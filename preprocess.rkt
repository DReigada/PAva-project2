#lang racket
(provide add-active-token def-active-token process-string)

(define *token-list* '())

(define (add-active-token token function)
    (set! *token-list* (cons (cons token function) *token-list*)))

(define-syntax-rule (def-active-token token parameters body)
    (add-active-token token (lambda parameters body)))

(define (process-string str) (proccess-aux str ""))


(define/match (proccess-aux str acc)
    [("" _) acc]
    [(str acc)
        (match (find-token str)
            ['()
                (proccess-aux (substring str 1) (string-append acc (substring str 0 1)))] 
            [(cons token token-function)
                (proccess-aux (token-function (substring str (string-length token))) acc)])])

(define (find-token str) (find-token-aux str *token-list*))

(define/match (find-token-aux str token-list)
    [(_ '()) '()]
    [(str (cons token-pair tail-list))
        (if (string-prefix? str (car token-pair))
            token-pair
            (find-token-aux str tail-list)
        )])


;; Defined active tokens

;; 2.1 Local Type Inference
(def-active-token "var" (str)
    (match str
        [(regexp #rx".*=.*new (.*)\\(.*\\).*;$" (list _ type))
            (string-append type str)]))