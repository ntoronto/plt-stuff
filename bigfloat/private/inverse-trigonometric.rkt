#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bf-pi.rkt" "binary-split.rkt")

(require/typed
 racket/base
 [integer-sqrt  (Natural -> Natural)])

(provide bfatan bfasin bfacos)

(: atan-terms-ub (bigfloat Exact-Positive-Integer
                           -> Exact-Positive-Integer))
;; Assumes x <= 0.5.
#|
Computes an upper bound on the number N of Taylor series terms needed to compute
atan within 1/2^p error.

Let M = 2*N-1. From atan's Taylor series expansion, we should solve

    M * log2(x) - log2(M) + p = 0

for M. But solving M * log2(x) + p = 0 is easier and gives a reasonably tight
upper bound that gets tighter as bf-bits increases.
|#
(define (atan-terms-ub x p)
  (def M (/ p (- (bfceiling-log2 x))))
  (def N (/ (add1 M) 2))
  (max 1 (ceiling N)))

(: taylor-sum/binary-split (bigfloat -> bigfloat))
(define (taylor-sum/binary-split x)
  (def prec (max 1 (- (bflog2-ulp x))))
  (def N (atan-terms-ub x prec))
  ;(printf "N = ~v~n" N)
  (def -x^2 (bfneg (bfsqr x)))
  (define-syntax-rule (p n) (if (zero? n) x -x^2))
  (define-syntax-rule (b n) (+ 1 (*2 n)))
  (def (values u v) (binary-split/pb* bigfloat Integer bf+ bf* * bf*i p b N))
  (bf/ u (bf v)))

(: bfatan* (bigfloat -> bigfloat))
(define (bfatan* x)
  (def k (max 0 (+ (bfceiling-log2 x) (integer-sqrt (bf-bits)))))
  ;(printf "k = ~v~n" k)
  (define extra-bits (max 0 (* 2 (- (bfceiling-log2 x)))))
  (parameterize ([bf-bits  (+ (bf-bits) (* 2 k) extra-bits)])
    (def y (let: loop : bigfloat ([y : bigfloat  x] [k : Natural  k])
             ;(printf "y = ~v~n" y)
             (cond [(zero? k)  y]
                   [else  (loop (bf/ (bfsub1 (bfsqrt (bfadd1 (bfsqr y)))) y)
                                (sub1 k))])))
    ((taylor-sum/binary-split y) . bf*2^ . k)))

(: bfatan (bigfloat -> bigfloat))
(define (bfatan x)
  (cond [(bfnegative? x)     (bfneg (bfatan (bfabs x)))]
        [(bfzero? x)         (bf 0)]
        [(x . bf= . (bf 1))  ((bf-pi) . bf/2^ . 2)]
        [(x . bf> . (bf 1))
         (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
           (bf- (bf/2 (bf-pi)) (bfatan* (bfinv x))))]
        [else
         (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
           (bfatan* x))]))

(: bfasin* (bigfloat -> bigfloat))
(define (bfasin* x)
  (cond [(bfnegative? x)  (bfneg (bfasin* (bfneg x)))]
        [(bfzero? x)      (bf 0)]
        [(bf= x (bf 1))   (bf/2 (bf-pi))]
        [else
         (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
           (bfatan* (bf/ x (bfsqrt (bf- (bf 1) (bfsqr x))))))]))

(: bfasin (bigfloat -> bigfloat))
(define (bfasin x)
  (when (or (x . bf> . (bf 1)) (x . bf< . (bf -1)))
    (raise-type-error 'bfasin "bigfloat in [-1,1]" x))
  (bfasin* x))

(: bfacos (bigfloat -> bigfloat))
(define (bfacos x)
  (when (or (x . bf> . (bf 1)) (x . bf< . (bf -1)))
    (raise-type-error 'bfacos "bigfloat in [-1,1]" x))
  (define y
    (parameterize
        ([bf-bits  (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))])
      (bfasin* x)))
  (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
    (bf- (bf/2 (bf-pi)) y)))
