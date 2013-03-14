#lang typed/racket

(require "set.rkt")

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

(define-for-syntax (undefined-outside-drbayes stx)
  (syntax-case stx ()
    [(name . args)  (raise-syntax-error 'drbayes "undefined outside DrBayes" #'name)]
    [_  (raise-syntax-error 'drbayes "undefined outside DrBayes" stx)]))

(define-syntax (strict-if stx) (undefined-outside-drbayes stx))
(define-syntax (strict-cond stx) (undefined-outside-drbayes stx))
(define-syntax (prim-if stx) (undefined-outside-drbayes stx))
(define-syntax (prim-cond stx) (undefined-outside-drbayes stx))
(define-syntax (lazy-if stx) (undefined-outside-drbayes stx))
(define-syntax (lazy-cond stx) (undefined-outside-drbayes stx))
(define-syntax (uniform stx) (undefined-outside-drbayes stx))
(define-syntax (normal stx) (undefined-outside-drbayes stx))
(define-syntax (cauchy stx) (undefined-outside-drbayes stx))
(define-syntax (boolean stx) (undefined-outside-drbayes stx))

(define tag?
  (λ: ([v : Value] [t : Set-Tag])
    (and (tagged? v) (eq? t (get-tag v)))))

(: tag (Value Set-Tag -> Value))
(define (tag v t)
  (tagged t v))

(: untag (Value Set-Tag -> Value))
(define (untag v t)
  (if (tag? v t)
      (get-val v)
      (raise-argument-error 'untag (symbol->string t) v)))
