#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base)
         racket/match
         "real-set.rkt"
         "bool-set.rkt"
         "null-set.rkt"
         "pair-set.rkt"
         "extremal-set.rkt"
         "union.rkt")

(struct: Bottom ([message : (Promise String)]) #:transparent)

(define-syntax bottom (make-rename-transformer #'Bottom))
(define-syntax bottom? (make-rename-transformer #'Bottom?))
(define-syntax bottom-message (make-rename-transformer #'Bottom-message))

(define-type Value (Rec Value (U Flonum Boolean Null (Pair Value Value) tagged-value)))
(define-type Maybe-Value (U Value Bottom))

(struct: tagged-value ([tag : Symbol] [value : Value]) #:transparent)

;; ===================================================================================================

(: value-tag (Value -> Tag))
(define (value-tag v)
  (cond [(flonum? v)   real-tag]
        [(boolean? v)  bool-tag]
        [(null? v)     null-tag]
        [(pair? v)     pair-tag]
        [else          (tagged-value-tag v)]))

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

(: value->singleton (Value -> Bot-Entry))
(define (value->singleton v)
  (cond [(flonum? v)   (flonum->singleton v)]
        [(boolean? v)  (boolean->singleton v)]
        [(null? v)     nulls]
        [(pair? v)     (pair->singleton v)]
        [else          (tagged-value->singleton v)]))

(: flonum->singleton (Flonum -> Bot-Basic))
(define (flonum->singleton x)
  (cond [(< -inf.0 x +inf.0)  (Nonextremal-Interval x x #t #t)]
        [else  (raise-argument-error 'flonum->singleton "rational Flonum" x)]))

(: boolean->singleton (Boolean -> Bot-Basic))
(define (boolean->singleton b)
  (if b trues falses))

(: pair->singleton ((Pair Value Value) -> Bot-Basic))
(define (pair->singleton x)
  ((inst Nonextremal-Pair-Rect Nonextremal-Set Universe)
   (value->singleton (car x))
   (value->singleton (cdr x))))

(: tagged-value->singleton (tagged-value -> Bot-Tagged))
(define (tagged-value->singleton v)
  (match-define (tagged-value tag val) v)
  (bot-tagged tag (value->singleton val)))
