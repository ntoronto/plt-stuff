#lang racket/base

(require (for-syntax racket/base))

(provide maybe-rename)

(define-syntax (maybe-rename stx)
  (syntax-case stx ()
    [(_ e)
     (let ([expanded  (local-expand #'e (syntax-local-context) #f)])
       (syntax-case expanded (define-values)
         [(define-values (x ...) expr)
          (with-syntax ([(y ...)  (generate-temporaries #'(x ...))])
            (syntax/loc stx
              (begin
                (define-syntax x (make-rename-transformer #'y)) ...
                (define-values (y ...) expr))))]
         [_  #'e]))]))

#;; doesn't work in the REPL:
(maybe-rename (define (count lst)
                (if (null? lst)
                    0
                    (add1 (count (cdr lst))))))
