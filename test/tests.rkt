#lang racket
 
(require rackunit
         "../preprocess.rkt")

;; Private comment
(def-active-token ";;" (str)
    (match (regexp-match-positions "\n" str)
        ((list (cons start end)) (substring str end))
        (else "")))

(define my-string1 ";;something\nhello\n;;hello\nthere")
(check-equal? (process-string my-string1) "hello\nthere" ";; test")


;; Eval expression
(define ns (make-base-namespace))
(def-active-token "//eval " (str)
    (call-with-input-string
        str
        (lambda (in)
            (string-append (~a (eval (read in) ns))
                            (port->string in)))))

(define my-string2 "if (curYear > 2018) {")
(check-equal? (process-string my-string2) my-string2 "eval test")


;; 2.1 Local Type Inference
(define type-inf-string1 "var x = new HashMap<String,Integer>();")
(define type-inf-expected-string1 "HashMap<String,Integer> x = new HashMap<String,Integer>();")
(check-equal? (process-string type-inf-string1) type-inf-expected-string1 "Type inference 1")


;; 2.2 String Interpolation
(define string-interpolation1 "String str = #\"First #{a}, then #{a+b}, finally #{b*c}.\";")
(define string-interpolation-expected1 "String str = \"First \" + (a) + \", then \" + (a+b) + \", finally \" + (b*c) + \".\";")
(check-equal? (process-string string-interpolation1) string-interpolation-expected1 "Type inference 1")