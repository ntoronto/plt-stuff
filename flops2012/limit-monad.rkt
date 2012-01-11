#lang racket

(provide return lift join bind lift2 accel)

#|
The computable limit monad

Computations in the computable limit monad have type

    m a = (ω => a).

where ω is the set of finite ordinals, for some type 'a' representing a metric
space. Not expressed in the type is the fact that the (lazy) sequences should
converge.

Computations cannot be run, but they can be evaluated at arbitarily high
indexes to get answers with arbitrarily small error.
|#

;; return : a => m a
(define ((return c) n) c)

;; lift : (a => b) => (m a => m b)
;; Proof obligations: f is continuous at (limit xs)
;;                    f is defined for all xs
(define ((lift f) xs)  (compose f xs))

;; join : m (m a) => m a
;; Proof obligation: (flip xss) converges uniformly
(define ((join xss) n)  ((xss n) n))

;; bind : m a => (a => m b) => m b
;; Proof obligations: f is continuous at (limit xs)
;;                    f is defined for all xs
;;                    flip (f ∘ xs) converges uniformly
(define (bind xs f)  (join ((lift f) xs)))

;; lift2 : (a => b => c) => m a => m b => m c
;; Proof obligations: f is continuous at limit (xs, ys)
(define ((lift2 f) xs ys)
  (bind xs (λ (x) (bind ys (λ (y) (return (f x y)))))))

;; accel : Q => m a => m a
;; Adjusts the speed of convergence; see rpi-ramanujan for example of use
(define ((accel m xs) n)
  (xs (floor (* m n))))
