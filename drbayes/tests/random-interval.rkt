#lang typed/racket

(require math/distributions
         "../private/set/interval.rkt"
         "../private/set/extremal-set.rkt")

(provide (all-defined-out))

(: random-element (All (A) ((Listof A) -> A)))
(define (random-element xs)
  (list-ref xs (random (length xs))))

;; Using a discrete distribution for interval endpoints makes it more likely that two endpoints from
;; different intervals will be the same but one open and the other closed
(define real-endpoint-dist
  (discrete-dist '(-inf.0 -4.0 -3.0 -2.0 -1.0 -0.0 0.0 1.0 2.0 3.0 4.0 +inf.0)))
(define random-reals
  '(-5.0 -4.0 -3.5 -3.0 -2.5 -2.0 -1.5 -1.0 -0.5 -0.0 0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 5.0))

(: random-interval ((Discrete-Dist Flonum) -> Interval))
(define (random-interval dist)
  (define a (sample dist))
  (define b (sample dist))
  (define a? ((random) . < . 0.5))
  (define b? ((random) . < . 0.5))
  (define I (interval (min a b) (max a b) a? b?))
  (if (empty-set? I) (random-interval dist) I))

(: random-real (Maybe-Interval* -> Flonum))
(define (random-real I)
  (cond [(empty-set? I)  +nan.0]
        [else  (random-element (filter (λ: ([x : Flonum]) (interval*-member? I x)) random-reals))]))

(: random-interval* (-> Interval*))
(define (random-interval*)
  (define I (random-interval real-endpoint-dist))
  (cond [((random) . < . 0.5)  I]
        [else  (interval*-union I (random-interval*))]))
