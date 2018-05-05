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

(define string-interpolation2 "String str = #\"First #{a}, then #{a+b}, finally #{b*c}.\" + #\"no replace #{noreplace}\";")
(define string-interpolation-expected2 "String str = \"First \" + (a) + \", then \" + (a+b) + \", finally \" + (b*c) + \".\" + \"no replace \" + (noreplace) + \"\";")
(check-equal? (process-string string-interpolation2) string-interpolation-expected2 "String interpolation 2")

;; 2.3 Type Aliases
(define type-alias-string1
#<<END
alias Cache = ConcurrentSkipListMap<String,List<Map<String,Object>>>;
public static Cache mergeCaches(Cache a, Cache b) {
    Cache temp = new Cache();
}
END
)
    
(define type-alias-expected1
#<<END
public static ConcurrentSkipListMap<String,List<Map<String,Object>>> mergeCaches(ConcurrentSkipListMap<String,List<Map<String,Object>>> a, ConcurrentSkipListMap<String,List<Map<String,Object>>> b) {
    ConcurrentSkipListMap<String,List<Map<String,Object>>> temp = new ConcurrentSkipListMap<String,List<Map<String,Object>>>();
}
END
)

(check-equal? (process-string type-alias-string1) type-alias-expected1 "Type alias 1")

;; Token mix - Type alias and type inference

(define token-mix-string1
#<<END
alias Cache = ConcurrentSkipListMap<String,List<Map<String,Object>>>;
public static Cache mergeCaches(Cache a, Cache b) {
    var temp = new Cache();
}
END
)

(define token-mix-expected1
#<<END
public static ConcurrentSkipListMap<String,List<Map<String,Object>>> mergeCaches(ConcurrentSkipListMap<String,List<Map<String,Object>>> a, ConcurrentSkipListMap<String,List<Map<String,Object>>> b) {
    ConcurrentSkipListMap<String,List<Map<String,Object>>> temp = new ConcurrentSkipListMap<String,List<Map<String,Object>>>();
}
END
)

(check-equal? (process-string token-mix-string1) token-mix-expected1 "Token mix 1")
