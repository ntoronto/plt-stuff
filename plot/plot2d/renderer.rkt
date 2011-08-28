#lang racket/base

(provide (all-defined-out))

(struct renderer2d (f x-ticks y-ticks bounds-fun x-min x-max y-min y-max)
        #:transparent)

(define (make-renderer2d f x-ticks y-ticks bounds-fun
                         x-min x-max y-min y-max)
  (when (and x-min x-max (x-min . >= . x-max))
    (error 'make-renderer2d
           "expected x-min < x-max; got x-min = ~e and x-max = ~e"
           x-min x-max))
  (when (and y-min y-max (y-min . >= . y-max))
    (error 'make-renderer2d
           "expected y-min < y-max; got y-min = ~e and y-max = ~e"
           y-min y-max))
  (renderer2d f x-ticks y-ticks bounds-fun x-min x-max y-min y-max))
