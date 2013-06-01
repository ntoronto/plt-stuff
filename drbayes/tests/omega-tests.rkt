#lang typed/racket

(require typed/rackunit
         math/distributions
         "../private/set/real-set.rkt"
         "../private/set/omega-value.rkt"
         "../private/set/omega-set.rkt"
         "rackunit-utils.rkt"
         "random-real-set.rkt")

(printf "starting...~n")

(define unit-endpoint-dist (discrete-dist '(0.0 0.1 0.2 0.3 0.5 0.7 0.8 0.9 1.0)))

(: random-omega-set (-> Omega-Set))
(define (random-omega-set)
  (cond [((random) . < . 0.1)  empty-tree-set]
        [else
         (let loop ([n 4])
           (define r (random))
           (cond [(or (r . < . 0.4) (zero? n))  full-tree-set]
                 [else
                  (let reject ()
                    (define I (random-real-set unit-endpoint-dist))
                    (cond [(or (empty-real-set? I) (reals? I))  (reject)]
                          [else
                           (omega-set I (omega-children-set (loop (- n 1)) (loop (- n 1))))]))]))]))

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    omega-set-subseteq?
    omega-set-join
    omega-set-intersect
    empty-tree-set
    full-tree-set
    random-omega-set)
   ((inst check-membership-lattice Omega-Set Omega)
    empty-tree-set?
    omega-set-member?
    omega-set-subseteq?
    omega-set-join
    omega-set-intersect
    random-omega-set
    (λ (A)
      (cond [(empty-tree-set? A)
             (raise-argument-error 'omega-set-sample-point "Nonempty-Omega-Set" A)]
            [else
             (omega-set-sample-point A)])))))
