#lang typed/racket

(require "../private/set/interval.rkt"
         "../private/set/extremal-set.rkt"
         "rackunit-utils.rkt"
         "random-interval.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-set-algebra interval*-member?
                      interval*-subseteq?
                      empty-set
                      real-interval
                      interval*-subtract
                      interval*-union
                      interval*-intersect
                      random-interval*
                      random-real)
   (check-bounded-lattice interval*-subseteq?
                          interval*-union
                          interval*-intersect
                          empty-set
                          real-interval
                          random-interval*)))
