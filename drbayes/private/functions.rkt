#lang typed/racket

(require math/distributions
         "types.rkt"
         "rect.rkt")

(provide (all-defined-out))

(define nonnegative? (λ: ([x : Real]) (x . >= . 0)))
(define nonpositive? (λ: ([x : Real]) (x . <= . 0)))
(define scale (λ: ([x : Real] [y : Real]) (real->double-flonum (* x y))))
(define translate (λ: ([x : Real] [y : Real]) (real->double-flonum (+ x y))))
(define (fail) (error 'fail "failure"))

(struct: bad ([value : Any]))

(: any->value (Any -> Value))
(define (any->value orig-v)
  (let loop ([v orig-v])
    (cond [(pair? v)     (cons (loop (car v)) (loop (cdr v)))]
          [(null? v)     null]
          [(real? v)     (real->double-flonum v)]
          [(boolean? v)  v]
          [else  (raise (bad v))])))

(: raise-bad-value (Any Any Any -> Nothing))
(define (raise-bad-value orig-v v stx)
  (define msg
    (cond [(eq? v orig-v)  (format "unimplemented constant ~e" v)]
          [else  (format "unimplemented constant ~e in ~e" v orig-v)]))
  (cond [stx  (raise (exn:fail:syntax (string-append "drbayes: " msg)
                                      (current-continuation-marks)
                                      (list (cast stx Syntax))))]
        [else  (error 'drbayes msg)]))

(: const (case-> (Any -> Value)
                 (Any Any -> Value)))
(define (const orig-v [stx #f])
  (with-handlers ([bad?  (λ (res) (raise-bad-value orig-v (bad-value res) stx))])
    (any->value orig-v)))

(define-syntax (strict-if stx) (raise-syntax-error 'strict-if "undefined" stx))
(define-syntax (strict-cond stx) (raise-syntax-error 'strict-cond "undefined" stx))

(define-syntax (lazy-if stx) (raise-syntax-error 'lazy-if "undefined" stx))
(define-syntax (lazy-cond stx) (raise-syntax-error 'lazy-cond "undefined" stx))

(define-syntax (uniform stx) (raise-syntax-error 'uniform "undefined" stx))
(define-syntax (normal stx) (raise-syntax-error 'normal "undefined" stx))
(define-syntax (cauchy stx) (raise-syntax-error 'cauchy "undefined" stx))
(define-syntax (boolean stx) (raise-syntax-error 'boolean "undefined" stx))
