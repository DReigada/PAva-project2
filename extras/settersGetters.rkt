#lang racket
(provide process-setters process-getters process-getters-and-setters)

(define (find-start/closing-bracket str)
    (match-let*-values ([((cons (cons first-position _) _)) (regexp-match-positions #px"\\{" str)]
                        [(_ bracket-position) (find-closing-bracket-aux (substring str first-position))])
        (values first-position (+ bracket-position first-position))))

; the string must start with: {
(define (find-closing-bracket-aux str)
    (for/fold  ([stack 0]
                [position 0])
                ([char str])
            (match (cons char stack)
                [(cons #\{ _)   (values (+ stack 1) (+ position 1))]
                [(cons #\} 1)   (values (- stack 1) position)]
                [(cons #\} _)   (values (- stack 1) (+ position 1))]
                [(cons _ 0)     (values stack position)]
                [(cons _ _)     (values stack (+ position 1))])))


;; expects the class body, between the parenthesis: { ... }
(define (find-class-attributes str start end)
    (regexp-match* #px"public ([\\S]*) ([^\\(\\s;]*)[^\\(]*?\n" str start (+ end 1) #:match-select cdr))


(define (add-margin str margin)
    (string-join (string-split str "\n") (string-append "\n" margin) #:before-first margin))


(define setter-template
#<<END
public void set~a(~a ~a) {
    this.~a = ~a;
}
END
)

(define (create-setter margin type name)
    (add-margin (format setter-template name type name name name) margin))


(define getter-template
#<<END
public ~a get~a() {
    return this.~a;
}
END
)

(define (create-getter margin type name)
    (add-margin (format getter-template type name name) margin))

;; get class limits
;; find attributes
;; generate getters and setters
;; insert getters and setters
(define (process-class-with-function str create-func)
    (match-define-values (class-start class-end) (find-start/closing-bracket str))
    (define attributes (find-class-attributes str class-start class-end))
    (define created-functions
        (for/fold ([acc ""]) ([attribute attributes])
            (match-define (list type name) attribute)
            (string-join (list acc (create-func "  " type name)) "\n\n")))
    (string-append (substring str 0 class-end) created-functions "\n" (substring str class-end)))


(define (process-setters str) (process-class-with-function str create-setter))
(define (process-getters str) (process-class-with-function str create-getter))
(define (process-getters-and-setters str) (process-class-with-function str
    (lambda (margin type name)
        (string-append (create-getter margin type name) "\n\n" (create-setter margin type name)))))
