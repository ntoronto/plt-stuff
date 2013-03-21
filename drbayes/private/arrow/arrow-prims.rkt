#lang typed/racket/base

(require racket/match
         racket/list
         racket/flonum
         racket/math
         math/distributions
         "../set.rkt"
         "arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Lists

(define null/arr (c/arr null))

(: list/arr (expression * -> expression))
(define (list/arr . es) (foldr pair/arr null/arr es))

(: strict-if/arr (expression expression expression -> expression))
(define (strict-if/arr c t f)
  (rcompose/arr (list/arr c t f)
                (prim-if/arr (ref/arr 0) (ref/arr 1) (ref/arr 2))))

;; ===================================================================================================
;; Primitive data type predicates

(define real?/arr (predicate/arr flonum? real-interval (top-rect real-tag empty-set)))
(define null?/arr (predicate/arr null? null-rect (top-rect null-tag empty-set)))
(define pair?/arr (predicate/arr cons? all-pairs (top-rect pair-tag empty-set)))
(define boolean?/arr (predicate/arr boolean? booleans (top-rect boolean-tag empty-set)))

;; ===================================================================================================
;; Monotone R -> R functions

(: scale/arr (Flonum -> expression))
(define (scale/arr y)
  (cond [(fl= y 0.0)  (c/arr 0.0)]
        [else  (monotone/arr 'scale/arr real-interval real-interval
                             (λ: ([x : Flonum]) (fl* x y))
                             (λ: ([z : Flonum]) (fl/ z y))
                             (y . fl> . 0.0))]))

(: translate/arr (Flonum -> expression))
(define (translate/arr y)
  (monotone/arr 'translate/arr real-interval real-interval
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

(define neg/arr (monotone/arr 'neg/arr real-interval real-interval flneg flneg #f))
(define exp/arr (monotone/arr 'exp/arr real-interval nonnegative-interval flexp fllog #t))
(define log/arr (monotone/arr 'log/arr nonnegative-interval real-interval fllog flexp #t))
(define sqrt/arr (monotone/arr 'sqrt/arr nonnegative-interval nonnegative-interval flsqrt flsqr #t))

(define asin/arr
  (monotone/arr 'asin/arr
                (Interval -1.0 1.0 #t #t)
                (Interval (/ pi -2.0) (/ pi 2.0) #t #t)
                flasin flsin #t))

(define acos/arr
  (monotone/arr 'acos/arr
                (Interval -1.0 1.0 #t #t)
                (Interval 0.0 pi #t #t)
                flacos flcos #f))

(define mono-sin/arr
  (monotone/arr 'mono-sin/arr
                (Interval (/ pi -2.0) (/ pi 2.0) #t #t)
                (Interval -1.0 1.0 #t #t)
                flsin flasin #t))

(define mono-cos/arr
  (monotone/arr 'mono-cos/arr
                (Interval 0.0 pi #t #t)
                (Interval -1.0 1.0 #t #t)
                flcos flacos #f))

(define pos-recip/arr
  (monotone/arr 'recip/arr positive-interval positive-interval flrecip flrecip #t))
(define neg-recip/arr
  (monotone/arr 'recip/arr negative-interval negative-interval flrecip flrecip #f))

(define pos-sqr/arr
  (monotone/arr 'sqr/arr nonnegative-interval nonnegative-interval flsqr flsqrt #t))
(define neg-sqr/arr
  (monotone/arr 'sqr/arr negative-interval positive-interval flsqr flneg-sqrt #f))

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

(define cauchy/arr (inverse-cdf/arr 'cauchy/arr real-interval cauchy-inv-cdf cauchy-cdf))
(define normal/arr (inverse-cdf/arr 'normal/arr real-interval normal-inv-cdf normal-cdf))

;; ===================================================================================================
;; Monotone R x R -> R functions

(define real-pair (pair-rect real-interval real-interval))

(define +/arr
  (monotone2d/arr '+/arr real-pair real-interval
                  fl+ #t #t
                  fl- #t #f
                  fl- #t #f))

(: neg-fl- (Flonum Flonum -> Flonum))
(define (neg-fl- z x) (fl- x z))

(define -/arr
  (monotone2d/arr '-/arr real-pair real-interval
                  fl- #t #f
                  fl+ #t #t
                  neg-fl- #f #t))

(define pos-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-interval nonnegative-interval) nonnegative-interval
                  fl* #t #t
                  fl/ #t #f
                  fl/ #t #f))

(define pos-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-interval negative-interval) nonpositive-interval
                  fl* #f #t
                  fl/ #f #t
                  fl/ #t #t))

(define neg-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-interval nonnegative-interval) nonpositive-interval
                  fl* #t #f
                  fl/ #t #t
                  fl/ #f #t))

(define neg-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-interval negative-interval) positive-interval
                  fl* #f #f
                  fl/ #f #f
                  fl/ #f #f))

(: recip-fl/ (Flonum Flonum -> Flonum))
(define (recip-fl/ z x) (fl/ x z))

(define pos-pos-div/arr
  (monotone2d/arr '//arr (pair-rect positive-interval positive-interval) positive-interval
                  fl/ #t #f
                  fl* #t #t
                  recip-fl/ #f #t))

(define pos-neg-div/arr
  (monotone2d/arr '//arr (pair-rect positive-interval negative-interval) negative-interval
                  fl/ #f #f
                  fl* #f #f
                  recip-fl/ #f #f))

(define neg-pos-div/arr
  (monotone2d/arr '//arr (pair-rect negative-interval positive-interval) negative-interval
                  fl/ #t #t
                  fl* #t #f
                  recip-fl/ #t #f))

(define neg-neg-div/arr
  (monotone2d/arr '//arr (pair-rect negative-interval negative-interval) positive-interval
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
                      negative-interval nonnegative-interval))

(define positive?/arr
  (real-predicate/arr 'positive?/arr (λ: ([x : Flonum]) (x . fl> . 0.0))
                      positive-interval nonpositive-interval))

(define nonpositive?/arr
  (real-predicate/arr 'nonpositive?/arr (λ: ([x : Flonum]) (x . fl<= . 0.0))
                      nonpositive-interval positive-interval))

(define nonnegative?/arr
  (real-predicate/arr 'nonnegative?/arr (λ: ([x : Flonum]) (x . fl>= . 0.0))
                      nonnegative-interval negative-interval))

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

(define -pi/arr (c/arr (- pi)))
(define -pi/2/arr (c/arr (* -0.5 pi)))
(define pi/2/arr (c/arr (* 0.5 pi)))
(define pi/arr (c/arr pi))

#;
(define (partial-cos x)
  (if (x . < . 0.0)
      (mono-cos (- x))
      (mono-cos x)))

(define partial-cos/arr
  (prim-if/arr negative?/arr
               (rcompose/arr neg/arr mono-cos/arr)
               mono-cos/arr))

#;
(define (partial-pos-sin x)
  (if ((+ x (* -0.5 pi)) . <= . 0.0)
      (mono-sin x)
      (let ([x  (+ x (- pi))])
        (if (x . <= . 0.0)
            (mono-sin (- x))
            (error 'bad)))))

#;
(define (partial-sin x)
  (if (x . < . 0.0)
      (- (partial-pos-sin (- x)))
      (partial-pos-sin x)))

(define partial-pos-sin/arr
  (prim-if/arr (rcompose/arr (translate/arr (* -0.5 pi)) nonpositive?/arr)
               mono-sin/arr
               (rcompose/arr
                (translate/arr (- pi))
                (prim-if/arr nonpositive?/arr
                             (rcompose/arr neg/arr mono-sin/arr)
                             bottom/arr))))

(define partial-sin/arr
  (prim-if/arr negative?/arr
               (rcompose/arr (rcompose/arr neg/arr partial-pos-sin/arr) neg/arr)
               partial-pos-sin/arr))

(define */arr
  (prim-if/arr (rcompose/arr (ref/arr 'fst) positive?/arr)
               (prim-if/arr (rcompose/arr (ref/arr 'snd) positive?/arr)
                            pos-pos-mul/arr
                            (prim-if/arr (rcompose/arr (ref/arr 'snd) negative?/arr)
                                         pos-neg-mul/arr
                                         (c/arr 0.0 real-pair)))
               (prim-if/arr (rcompose/arr (ref/arr 'fst) negative?/arr)
                            (prim-if/arr (rcompose/arr (ref/arr 'snd) positive?/arr)
                                         neg-pos-mul/arr
                                         (prim-if/arr (rcompose/arr (ref/arr 'snd) negative?/arr)
                                                      neg-neg-mul/arr
                                                      (c/arr 0.0 real-pair)))
                            (c/arr 0.0 real-pair)))
  #;
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
