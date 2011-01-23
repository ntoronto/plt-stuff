#lang typed/racket/base

(require (for-syntax racket/base)
         "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt")

(require/typed
 racket/base
 [raise-type-error  (Symbol String Integer Any * -> Nothing)]
 [abs  (Exact-Rational -> Exact-Rational)])

; except-out necessary because of a bug in TR
(provide (except-out (all-defined-out) abs raise-type-error))

(: exact-relative-error (bigfloat Exact-Rational -> Exact-Rational))
(define (exact-relative-error x r)
  (/ (abs (- (bigfloat->rational x) r)) r))

(: exact-ulp-error (bigfloat Exact-Rational -> Exact-Rational))
(define (exact-ulp-error x r)
  (/ (abs (- (bigfloat->rational x) r))
     (bfulp x)))

(: bfrelative-error (bigfloat bigfloat -> bigfloat))
;; Note: this assumes bf/ is accurate enough to measure error in ulps. It should
;; be accurate enough if y has a lot more bits than x.
(define (bfrelative-error x y)
  (when ((bigfloat-bits x) . > . (bigfloat-bits y))
    (raise-type-error
     'bfrelative-error (format "bigfloat with bits >= ~e" (bigfloat-bits x))
     1 x y))
  (with-bf-bits (+ 2 (* 2 (bigfloat-bits y)))
    (bf/ (bfabs (bf- x y)) y)))

(: bfulp-error (bigfloat bigfloat -> bigfloat))
(define (bfulp-error x y)
  (when ((bigfloat-bits x) . > . (bigfloat-bits y))
    (raise-type-error
     'bfulp-error (format "bigfloat with bits >= ~e" (bigfloat-bits x))
     1 x y))
  (with-bf-bits (+ 2 (* 2 (bigfloat-bits y)))
    ((bfabs (bf- x y)) . bf/2^ . (bflog2-ulp x))))

(define-syntax (bftest-error stx)
  (syntax-case stx ()
    [(_ expr)  (syntax/loc stx (bftest-error expr (* 2 (bf-bits))))]
    [(_ expr bits)
     (syntax/loc stx
       (bfulp-error expr (parameterize ([bf-bits bits]) expr)))]))
