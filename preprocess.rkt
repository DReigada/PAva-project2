#lang racket
(provide add-active-token def-active-token process-string)

(define *token-list* '())

(define (add-active-token token function)
    (set! *token-list* (cons (cons token function) *token-list*)))

(define-syntax-rule (def-active-token token parameters body)
    (add-active-token token (lambda parameters body)))

(define (process-string str) (proccess-aux str ""))


(define (string-ends-with-non-whitespace? str)
    (and (> (string-length str) 0) (regexp-match #px"\\w" (substring str (- (string-length str) 1)))))

(define (proccess-aux str acc)
    (cond
        [(equal? str "") acc]
        [(string-ends-with-non-whitespace? acc)
            (proccess-aux (substring str 1) (string-append acc (substring str 0 1)))]
        [else
            (match (find-token str)
                ['()
                    (proccess-aux (substring str 1) (string-append acc (substring str 0 1)))] 
                [(cons token token-function)
                    (proccess-aux (token-function (substring str (string-length token))) acc)])]))

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
(def-active-token "var " (str) 
    (match str
            [(regexp #px".*?=.*?new (.*?)\\(.*\\).*" (list _ type))
                (string-append type " " str)]
            [str (error (string-append "Invalid `var` syntax in: var " str))]))

;; 2.2 String Interpolation
(define (match-string-start-end str)
    (car (regexp-match-positions #px"\"(?:(?=(\\\\?))\\1.)*?\"" str)))

(def-active-token "#" (str)
    (match-let* ([(cons start end) (match-string-start-end str)]
                 [first-string (substring str start end)]
                 [tail-string (substring str end)])
        (string-append 
            (regexp-replace* #rx"\\#\\{(.*?)\\}" first-string "\" + (\\1) + \"")
            tail-string)))

;; 2.3 Type Aliases
(def-active-token "alias " (str)
    (match-let* ([(cons (cons first-alias _) _) (regexp-match-positions ";" str)]
                [first-line (substring str 0 (+ first-alias 1))]
                [remaining-lines (substring str (+ 1 first-alias))])
        (match first-line
            [(regexp #px"[\\s]*(.*?)[\\s]*=[\\s]*(.*?)[\\s]*;" (list _ alias-name alias-type))
                (regexp-replace*
                    (pregexp (string-append "(?<![\\w])" alias-name "(?![\\w])"))
                    remaining-lines
                    alias-type)])))
