#lang typed/racket/base

(require racket/match
         racket/list
         racket/flonum
         math/distributions
         "rect.rkt"
         "arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Lists

(define null/arr (c/arr null))
(define null?/arr (predicate/arr null? null-rect (Join-Rect reals #f universal-pair 'tf)))

(: list/arr (expression * -> expression))
(define (list/arr . es) (foldr pair/arr null/arr es))

;; ===================================================================================================
;; Monotone R -> R functions

(: scale/arr (Flonum -> expression))
(define (scale/arr y)
  (cond [(fl= y 0.0)  (c/arr 0.0)]
        [else  (monotone/arr 'scale/arr reals reals
                             (λ: ([x : Flonum]) (fl* x y))
                             (λ: ([z : Flonum]) (fl/ z y))
                             (y . fl> . 0.0))]))

(: translate/arr (Flonum -> expression))
(define (translate/arr y)
  (monotone/arr 'translate/arr reals reals
                (λ: ([x : Flonum]) (fl+ x y))
                (λ: ([z : Flonum]) (fl- z y))
                #t))

(: flneg (Flonum -> Flonum))
(define (flneg x) (fl* -1.0 x))

(: flsqr (Flonum -> Flonum))
(define (flsqr x) (fl* x x))

(: flrecip (Flonum -> Flonum))
(define (flrecip x) (fl/ 1.0 x))

(: flneg-sqrt (Flonum -> Flonum))
(define (flneg-sqrt x) (- (flsqrt x)))

(define neg/arr (monotone/arr 'neg/arr reals reals flneg flneg #f))
(define exp/arr (monotone/arr 'exp/arr reals nonnegative-reals flexp fllog #t))
(define log/arr (monotone/arr 'log/arr nonnegative-reals reals fllog flexp #t))
(define sqrt/arr (monotone/arr 'sqrt/arr nonnegative-reals nonnegative-reals flsqrt flsqr #t))
(define pos-recip/arr (monotone/arr 'recip/arr positive-reals positive-reals flrecip flrecip #t))
(define neg-recip/arr (monotone/arr 'recip/arr negative-reals negative-reals flrecip flrecip #f))
(define pos-sqr/arr (monotone/arr 'sqr/arr nonnegative-reals nonnegative-reals flsqr flsqrt #t))
(define neg-sqr/arr (monotone/arr 'sqr/arr negative-reals positive-reals flsqr flneg-sqrt #f))

(: inverse-cdf/arr (Symbol Interval (Flonum -> Flonum) (Flonum -> Flonum) -> expression))
(define (inverse-cdf/arr name range inv-cdf cdf)
  (monotone/arr name unit-interval range inv-cdf cdf #t))

(: cauchy-inv-cdf (Flonum -> Flonum))
(define (cauchy-inv-cdf p)
  (flcauchy-inv-cdf 0.0 1.0 p #f #f))

(: cauchy-cdf (Flonum -> Flonum))
(define (cauchy-cdf x)
  (flcauchy-cdf 0.0 1.0 x #f #f))

(: normal-inv-cdf (Flonum -> Flonum))
(define (normal-inv-cdf p)
  (flnormal-inv-cdf 0.0 1.0 p #f #f))

(: normal-cdf (Flonum -> Flonum))
(define (normal-cdf x)
  (flnormal-cdf 0.0 1.0 x #f #f))

(define cauchy/arr (inverse-cdf/arr 'cauchy/arr reals cauchy-inv-cdf cauchy-cdf))
(define normal/arr (inverse-cdf/arr 'normal/arr reals normal-inv-cdf normal-cdf))

;; ===================================================================================================
;; Monotone R x R -> R functions

(define +/arr
  (monotone2d/arr '+/arr (pair-rect reals reals) reals
                  fl+ #t #t
                  fl- #t #f
                  fl- #t #f))

(: neg-fl- (Flonum Flonum -> Flonum))
(define (neg-fl- z x) (fl- x z))

(define -/arr
  (monotone2d/arr '-/arr (pair-rect reals reals) reals
                  fl- #t #f
                  fl+ #t #t
                  neg-fl- #f #t))

(define pos-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-reals nonnegative-reals) nonnegative-reals
                  fl* #t #t
                  fl/ #t #f
                  fl/ #t #f))

(define pos-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-reals negative-reals) nonpositive-reals
                  fl* #f #t
                  fl/ #f #t
                  fl/ #t #t))

(define neg-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-reals nonnegative-reals) nonpositive-reals
                  fl* #t #f
                  fl/ #t #t
                  fl/ #f #t))

(define neg-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-reals negative-reals) positive-reals
                  fl* #f #f
                  fl/ #f #f
                  fl/ #f #f))

(: recip-fl/ (Flonum Flonum -> Flonum))
(define (recip-fl/ z x) (fl/ x z))

(define pos-pos-div/arr
  (monotone2d/arr '//arr (pair-rect positive-reals positive-reals) positive-reals
                  fl/ #t #f
                  fl* #t #t
                  recip-fl/ #f #t))

(define pos-neg-div/arr
  (monotone2d/arr '//arr (pair-rect positive-reals negative-reals) negative-reals
                  fl/ #f #f
                  fl* #f #f
                  recip-fl/ #f #f))

(define neg-pos-div/arr
  (monotone2d/arr '//arr (pair-rect negative-reals positive-reals) negative-reals
                  fl/ #t #t
                  fl* #t #f
                  recip-fl/ #t #f))

(define neg-neg-div/arr
  (monotone2d/arr '//arr (pair-rect negative-reals negative-reals) positive-reals
                  fl/ #f #t
                  fl* #f #t
                  recip-fl/ #t #t))

;; ===================================================================================================
;; Real predicates

(: real-predicate/arr (Symbol (Flonum -> Boolean) Interval Interval -> expression))
(define (real-predicate/arr name p? true-ivl false-ivl)
  (predicate/arr (λ: ([γ : Value])
                   (if (flonum? γ) (p? γ) (raise-argument-error name "Flonum" γ)))
                 true-ivl false-ivl))

(define negative?/arr
  (real-predicate/arr 'negative?/arr (λ: ([x : Flonum]) (x . fl< . 0.0))
                      negative-reals nonnegative-reals))

(define positive?/arr
  (real-predicate/arr 'positive?/arr (λ: ([x : Flonum]) (x . fl> . 0.0))
                      positive-reals nonpositive-reals))

(define nonpositive?/arr
  (real-predicate/arr 'nonpositive?/arr (λ: ([x : Flonum]) (x . fl<= . 0.0))
                      nonpositive-reals positive-reals))

(define nonnegative?/arr
  (real-predicate/arr 'nonnegative?/arr (λ: ([x : Flonum]) (x . fl>= . 0.0))
                      nonnegative-reals negative-reals))

(define lt/arr (rcompose/arr -/arr negative?/arr))
(define gt/arr (rcompose/arr -/arr positive?/arr))
(define lte/arr (rcompose/arr -/arr nonpositive?/arr))
(define gte/arr (rcompose/arr -/arr nonnegative?/arr))

;; ===================================================================================================
;; Non-monotone functions

(define sqr/arr
  (prim-if/arr negative?/arr neg-sqr/arr pos-sqr/arr))

(define recip/arr
  (prim-if/arr positive?/arr
               pos-recip/arr
               (prim-if/arr negative?/arr
                            neg-recip/arr
                            bottom/arr)))

(define */arr
  (prim-if/arr (rcompose/arr (ref/arr 'fst) negative?/arr)
               (prim-if/arr (rcompose/arr (ref/arr 'snd) negative?/arr)
                            neg-neg-mul/arr
                            neg-pos-mul/arr)
               (prim-if/arr (rcompose/arr (ref/arr 'snd) negative?/arr)
                            pos-neg-mul/arr
                            pos-pos-mul/arr)))

(define //arr
  (prim-if/arr (rcompose/arr (ref/arr 'snd) positive?/arr)
               (prim-if/arr (rcompose/arr (ref/arr 'fst) positive?/arr)
                            pos-pos-div/arr
                            (prim-if/arr (rcompose/arr (ref/arr 'fst) negative?/arr)
                                         neg-pos-div/arr
                                         (c/arr 0.0)))
               (prim-if/arr (rcompose/arr (ref/arr 'snd) negative?/arr)
                            (prim-if/arr (rcompose/arr (ref/arr 'fst) positive?/arr)
                                         pos-neg-div/arr
                                         (prim-if/arr (rcompose/arr (ref/arr 'fst)
                                                                    negative?/arr)
                                                      neg-neg-div/arr
                                                      (c/arr 0.0)))
                            bottom/arr)))
