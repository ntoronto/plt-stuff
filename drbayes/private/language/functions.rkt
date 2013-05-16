#lang typed/racket

(require racket/flonum
         math/flonum
         math/distributions
         "../set.rkt")

(provide (all-defined-out))

(: any->value (Any (Any -> Any) (Any -> Nothing) -> Value))
(define (any->value v f fail)
  (let loop ([v v])
    (define x (f v))
    (cond [(or (null? x) (boolean? x))  x]
          [(pair? x)  (cons (loop (car x)) (loop (cdr x)))]
          [(real? x)  (real->double-flonum x)]
          [(tagged-value? x)
           (define tag (tagged-value-tag x))
           (if (symbol? tag)
               (tagged-value tag (loop (tagged-value-value x)))
               (fail v))]
          [else  (fail v)])))

(: const (case-> (Any -> Value)
                 (Any Any -> Value)))
(define (const orig-v [stx #f])
  (any->value
   orig-v
   (λ (v) v)
   (λ (v)
     (define msg
       (cond [(eq? v orig-v)  (format "illegal constant ~e" v)]
             [else  (format "illegal constant ~e in ~e" v orig-v)]))
     (cond [stx   (raise (exn:fail:syntax (string-append "drbayes: " msg)
                                          (current-continuation-marks)
                                          (list (cast stx Syntax))))]
           [else  (error 'drbayes msg)]))))

(: syntax-const ((Syntaxof Any) -> Value))
(define (syntax-const orig-stx)
  (any->value
   orig-stx
   (λ (maybe-stx) (if (syntax? maybe-stx) (syntax-e maybe-stx) maybe-stx))
   (λ (maybe-stx) (raise-syntax-error 'drbayes "illegal constant" orig-stx maybe-stx))))

(define-syntax-rule (strict-if e ...) (if e ...))
(define-syntax-rule (strict-cond e ...) (cond e ...))
(define-syntax-rule (prim-if e ...) (if e ...))
(define-syntax-rule (prim-cond e ...) (cond e ...))
(define-syntax-rule (lazy-if e ...) (if e ...))
(define-syntax-rule (lazy-cond e ...) (if e ...))

(define nonnegative? (λ: ([x : Real]) ((fl x) . >= . 0.0)))
(define nonpositive? (λ: ([x : Real]) ((fl x) . <= . 0.0)))
(define scale (λ: ([x : Real] [y : Real]) (* (fl x) (fl y))))
(define translate (λ: ([x : Real] [y : Real]) (+ (fl x) (fl y))))
(define (fail) (error 'fail "failure"))

(: uniform (case-> (-> Flonum)
                   (Real -> Flonum)
                   (Real Real -> Flonum)))
(define uniform
  (case-lambda
    [()  (random)]
    [(b)  (flvector-ref (fluniform-sample 0.0 (fl b) 1) 0)]
    [(a b)  (flvector-ref (fluniform-sample (fl a) (fl b) 1) 0)]))

(: normal (case-> (-> Flonum)
                  (Real -> Flonum)
                  (Real Real -> Flonum)))
(define (normal [μ 0.0] [σ 1.0])
  (flvector-ref (flnormal-sample (fl μ) (fl σ) 1) 0))

(: cauchy (case-> (-> Flonum)
                  (Real -> Flonum)
                  (Real Real -> Flonum)))
(define (cauchy [m 0.0] [s 1.0])
  (flvector-ref (flcauchy-sample (fl m) (fl s) 1) 0))

(: boolean (Real -> Boolean))
(define (boolean p)
  ((random) . < . (fl p)))

(: partial-sin (Real -> Flonum))
(define (partial-sin orig-x)
  (define x (fl orig-x))
  (if (and (x . >= . (- pi)) (x . <= . pi))
      (sin x)
      (raise-argument-error 'partial-sin "Real in [-π,π]" orig-x)))

(: partial-cos (Real -> Flonum))
(define (partial-cos orig-x)
  (define x (fl orig-x))
  (if (and (x . >= . (- pi)) (x . <= . pi))
      (cos x)
      (raise-argument-error 'partial-cos "Real in [-π,π]" orig-x)))

(define tag?
  (λ: ([v : Value] [t : Tag])
    (and (tagged-value? v) (eq? t (tagged-value-tag v)))))

(: tag (Value Tag -> Value))
(define (tag v t)
  (tagged-value t v))

(: untag (Value Tag -> Value))
(define (untag v t)
  (if (tag? v t)
      (tagged-value-value v)
      (raise-argument-error 'untag (symbol->string t) v)))
