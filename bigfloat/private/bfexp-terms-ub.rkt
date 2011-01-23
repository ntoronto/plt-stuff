#lang typed/racket/base

(require (except-in racket/flonum fl->exact-integer)
         "utils.rkt" "bf.rkt" "simple.rkt")

(require/typed
 racket/flonum
 [fl->exact-integer  (Float -> Integer)])

(provide bfexp-terms-ub)

(: bfexp-terms-ub (bigfloat Exact-Positive-Integer
                            -> Exact-Positive-Integer))
;; Computes an upper bound on the number of Taylor series terms needed to
;; compute (exp x) with p fractional bits.
;; Main idea: Find an n for which x^n/n! contributes nothing detectable to the
;; power series. In other words, find an n where
;;     x^n/n! <= 1/2^p
;;     log(x^n/n!) <= log(1/2^p)
;;     n*log(x) - log(n!) <= -p*log(2)
;;     n*log(x) - log(n!) + p*log(2) <= 0
;; There's not a nice way to find a root for the left side. But there is for
;;     f(n) = n*log(x) - (n*log(n)-n) + p*log(2)
;; With this, n is guaranteed to be bigger than necessary, but not much, because
;; n*log(n)-n is a very tight lower bound on log(n!). The first derivative is
;;     f'(n) = log(x)-log(n)
;; We use Newton-Raphson until the change is <= 1 and take the ceiling. With the
;; first estimate we use, N-R tends to converge after two iterations when
;; p = 128. It takes up to five on large p (such as 2^32).
(define (bfexp-terms-ub x p)
  (cond
    [(or (x . bf< . (bf 0)) (x . bf> . (bf 10)))
     (raise-type-error 'bfexp-terms-ub "exact-rational in [0,10]" x)]
    [(p . > . (2^ 32))
     (error 'bfexp-terms-ub
            "Are you kidding? p = ~e? (Less than 1/2^2^32 epsilon??)" p)]
    [(bfzero? x)  1]
    [else
     (def p*log-2 (fl* (->fl p) 0.6931471805599453))  ; 0.69... = (log 2)
     (def log-x (fl* (exact->inexact (bfceiling-log2 x)) 0.6931471805599453))
     (def n0 (fl+ (fl* 4.0 (flsqrt (fl+ (fl* 4.0 (fl* log-x log-x)) p*log-2)))
                  (fl* 8.0 log-x)))
     (when (n0 . = . +inf.0)
       (error 'bfexp "cannot compute an upper bound on MacLaurin series terms: overflowed a float"))
     (let: loop : Exact-Positive-Integer ([ni : Float  n0] [i : Integer  0])
       (def log-ni (fllog ni))
       (def ni+1 (- ni (/ (+ (* ni (- (+ 1.0 log-x) log-ni)) p*log-2)
                          (- log-x log-ni))))
       (cond [(<= (abs (- ni+1 ni)) 1.0)
              (add1 (abs (fl->exact-integer (flfloor ni+1))))]
             [else  (loop ni+1 (add1 i))]))]))
