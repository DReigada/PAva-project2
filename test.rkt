#lang racket
 
(require "preprocess.rkt")

(define input
#<<END
@Setters
@Getters
public class Test {
    public String str;
    public int int;
}
END
)

(define output
#<<END
public class Test {
    public String str;
    public int num;

    public void setstr(String str){
        this.str = str;
    }

    public void setnum(String num){
        this.num = num;
    }

    public String getstr(){
        return str;
    }

    public int getint(){
        return int;
    }
}
END
)

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


(define setter-template
#<<END
public void set~a(~a ~a) {
    this.~a = ~a;
}
END
)

(define (create-setter type name)
    (format setter-template name type name name name))


(define getter-template
#<<END
public ~a get~a() {
    return this.~a;
}
END
)

(define (create-getter type name)
    (format getter-template type name name))

;; get class limits
;; find attributes
;; generate getters and setters
;; insert getters and setters


(define (test str)
    (match-define-values (class-start class-end) (find-start/closing-bracket str))
    (define attributes (find-class-attributes str class-start class-end))
    (for/fold ([acc ""]) ([attribute attributes])
        (match-define (list type name) attribute)
        (string-join (list acc (create-setter type name) (create-getter type name)) "\n\n")))

(displayln (test input))
