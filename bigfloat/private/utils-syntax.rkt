#lang racket/base

(require (for-syntax racket/base)
         (only-in racket/match match-define))

(provide def cond*)

(define-syntax (def stx)
  (syntax-case stx (values)
    [(_ x e0 e ...)
     (identifier? #'x)
     (syntax/loc stx
       (define x (let () e0 e ...)))]
    [(_ (values x ...) e0 e ...)
     (andmap identifier? (syntax->list #'(x ...)))
     (syntax/loc stx
       (define-values (x ...) (let () e0 e ...)))]
    [(_ (values pat ...) e0 e ...)
     (syntax/loc stx
       (match-define (list pat ...)
                     (call-with-values (lambda () e0 e ...) list)))]
    [(_ pat e0 e ...)
     (syntax/loc stx
       (match-define pat (let () e0 e ...)))]))

(define-syntax (cond* stx)
  (syntax-case stx (else)
    [(_ with expr clause ...)
     (equal? (syntax->datum #'with) '#:with)
     (syntax/loc stx
       (let () expr (cond* clause ...)))]
    [(_ [else else-expr0 else-expr ...])
     (syntax/loc stx
       (let () else-expr0 else-expr ...))]
    [(_ [test-expr then-expr0 then-expr ...] clause ...)
     (syntax/loc stx
       (if test-expr
           (let () then-expr0 then-expr ...)
           (cond* clause ...)))]
    [(_)  (syntax/loc stx
            (void))]))
