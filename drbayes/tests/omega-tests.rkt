#lang typed/racket

(require typed/rackunit
         math/distributions
         "../private/set.rkt"
         "rackunit-utils.rkt"
         "random-real-set.rkt")

(printf "starting...~n")

(define unit-endpoint-dist (discrete-dist '(0.0 0.1 0.2 0.3 0.5 0.7 0.8 0.9 1.0)))

(: random-omega-rect (-> (U Empty-Set Omega-Rect)))
(define (random-omega-rect)
  (cond [((random) . < . 0.1)  empty-set]
        [else
         (let loop ([n 4])
           (define r (random))
           (cond [(or (r . < . 0.4) (zero? n))  omega-leaf]
                 [else
                  (let reject ()
                    (define I (random-real-set unit-endpoint-dist))
                    (cond [(or (empty-real-set? I) (full-real-set? I))  (reject)]
                          [else  
                           (omega-rect-node I (loop (- n 1)) (loop (- n 1)))]))]))]))

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    omega-rect-subseteq?
    omega-rect-join
    omega-rect-intersect
    empty-set
    omega-leaf
    random-omega-rect)
   #;
   ((inst check-membership-lattice Maybe-Omega-Rect Omega)
    equal?
    empty-set?
    omega-rect-member?
    omega-rect-subseteq?
    omega-rect-join
    omega-rect-intersect
    random-omega-rect
    omega-rect-sample-point)))
