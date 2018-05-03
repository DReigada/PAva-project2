#lang racket
 
(require rackunit
         "../preprocess.rkt")


(def-active-token ";;" (str)
    (match (regexp-match-positions "\n" str)
        ((list (cons start end)) (substring str end))
        (else "")))

(define my-string1 ";;something\nhello\n;;hello\nthere")
(check-equal? (process-string my-string1) "hello\nthere" ";; test")

(define ns (make-base-namespace))

(def-active-token "//eval " (str)
    (call-with-input-string
        str
        (lambda (in)
            (string-append (~a (eval (read in) ns))
                            (port->string in)))))

(define my-string2 "if (curYear > 2018) {")
(check-equal? (process-string my-string2) my-string2 "eval test")


(define type-inf-string1 "var x = new HashMap<String,Integer>();")
(define type-inf-expected-string1 "HashMap<String,Integer> x = new HashMap<String,Integer>();")
(check-equal? (process-string type-inf-string1) type-inf-expected-string1 "Type inference 1")
