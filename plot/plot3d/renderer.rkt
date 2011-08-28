#lang racket/base

(provide (all-defined-out))

(struct renderer3d (f x-ticks y-ticks z-ticks bounds-fun
                      x-min x-max y-min y-max z-min z-max)
        #:transparent)

(define (make-renderer3d f x-ticks y-ticks z-ticks bounds-fun
                         x-min x-max y-min y-max z-min z-max)
  (when (and x-min x-max (x-min . >= . x-max))
    (error 'make-renderer3d
           "expected x-min < x-max; got x-min = ~e and x-max = ~e"
           x-min x-max))
  (when (and y-min y-max (y-min . >= . y-max))
    (error 'make-renderer3d
           "expected y-min < y-max; got y-min = ~e and y-max = ~e"
           y-min y-max))
  (when (and z-min z-max (z-min . >= . z-max))
    (error 'make-renderer3d
           "expected z-min < z-max; got z-min = ~e and z-max = ~e"
           z-min z-max))
  (renderer3d f x-ticks y-ticks z-ticks bounds-fun
              x-min x-max y-min y-max z-min z-max))
