#lang racket
(require "preprocess.rkt")

(add-active-token "++" identity)
(def-active-token "--" (str) (+ str 1))

(def-active-token ";;" (str)
    (or (for/or ((c (in-string str))
            (i (in-naturals)))
        (and (char=? c #\newline)
            (substring str (+ i 1))))
    ""))


(define my-string ";;asdfasdfa\nhello\n;;sdf\nasdfsadf")

(display (process-string my-string))
