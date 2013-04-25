#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base)
         racket/match
         "interval.rkt"
         "boolean-rect.rkt"
         "union.rkt")

(struct: Bottom ([message : (Promise String)]) #:transparent)

(define-syntax bottom (make-rename-transformer #'Bottom))
(define-syntax bottom? (make-rename-transformer #'Bottom?))
(define-syntax bottom-message (make-rename-transformer #'Bottom-message))

(define-type Value (Rec Value (U Flonum Boolean Null (Pair Value Value) (Tagged Set-Tag Value))))
(define-type Maybe-Value (U Value Bottom))

(define-type Tagged-Value (Tagged Set-Tag Value))

;; ===================================================================================================

(: value-tag (case-> ((U Flonum Null Pair Boolean) -> Rect-Tag)
                     (Tagged-Value -> Set-Tag)
                     (Value -> (U Rect-Tag Set-Tag))))
(define (value-tag v)
  (cond [(tagged? v)  (get-tag v)]
        [(flonum? v)  real-tag]
        [(null? v)    null-tag]
        [(pair? v)    pair-tag]
        [(boolean? v)  boolean-tag]))

;; ===================================================================================================
;; Ref

(: value-pair-ref (Value Pair-Index -> Value))
(define (value-pair-ref v j)
  (cond [(pair? v)  (pair-ref v j)]
        [else  (raise-argument-error 'value-pair-ref "Pair" 0 v j)]))

(: pair-ref ((Pair Value Value) Pair-Index -> Value))
(define (pair-ref x j)
  (match-define (cons x1 x2) x)
  (cond [(eq? j 'fst)  x1]
        [(eq? j 'snd)  x2]
        [(zero? j)     x1]
        [else  (value-pair-ref x2 (- j 1))]))

;; ===================================================================================================
;; Singleton

(: value->singleton (Value -> Nonextremal-Set))
(define (value->singleton v)
  (cond [(flonum? v)   (flonum->singleton v)]
        [(null? v)     null-set]
        [(pair? v)     (pair->singleton v)]
        [(boolean? v)  (boolean->singleton v)]
        [else          (tagged-value->singleton v)]))

(: pair->singleton ((Pair Value Value) -> Rect))
(define (pair->singleton x1x2)
  (match-define (cons x1 x2) x1x2)
  (pair-rect (value->singleton x1)
             (value->singleton x2)))

(: flonum->singleton (Flonum -> Rect))
(define (flonum->singleton x)
  (cond [(< -inf.0 x +inf.0)  (Interval x x #t #t)]
        [else  (raise-argument-error 'flonum->singleton "rational Flonum" x)]))

(: boolean->singleton (Boolean -> Rect))
(define (boolean->singleton b)
  (if b trues falses))

(: tagged-value->singleton (Tagged-Value -> Bot-Set))
(define (tagged-value->singleton v)
  (match-define (Tagged tag val) v)
  (bot-set tag (value->singleton val)))
