#lang racket
 
(require rackunit
         "../preprocess.rkt")

(define (process-file file-path output-suffix)
    (display-to-file
        (process-string (file->string file-path #:mode 'text))
        (string-append (path->string file-path) output-suffix)
        #:mode 'text #:exists 'replace))


(define (process-directory directory-path input-suffix output-suffix)
    (for-each
        (lambda (file-path) (process-file file-path output-suffix))
        (filter
            (lambda (file-path) (string-suffix? (path->string file-path) input-suffix))
            (directory-list directory-path #:build? #t))))


(define (verify-file expected-file-path actual-file-path)
    (check-equal? (file->string expected-file-path) (file->string actual-file-path) actual-file-path))

(define (verify-directory directory-path)
    (for-each
        (lambda (input-file-path)
            (verify-file 
                (regexp-replace #px"\\.in$" (path->string input-file-path) ".out")
                (regexp-replace #px"\\.in$" (path->string input-file-path) ".in.out")))
        (filter
            (lambda (file-path) (string-suffix? (path->string file-path) ".in"))
            (directory-list directory-path #:build? #t))))



(define (process-and-verify-directory directory)
    (process-directory directory ".in" ".out")
    (verify-directory directory))

(process-and-verify-directory "provided/string-interpolation")
(process-and-verify-directory "provided/type-alias")
(process-and-verify-directory "provided/mixed-tokens")
(process-and-verify-directory "provided/type-inference")
