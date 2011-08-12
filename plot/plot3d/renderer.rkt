#lang racket/base

(require racket/gui racket/match
         "../common/math.rkt")

(provide (all-defined-out))

(struct renderer3d
        (f x-ticks y-ticks z-ticks x-min x-max y-min y-max z-min z-max)
        #:transparent
        #:guard
        (λ (f x-ticks y-ticks z-ticks x-min x-max y-min y-max z-min z-max _)
          (values (λ (area)
                    (send area clip-to-bounds
                          x-min x-max y-min y-max z-min z-max)
                    (f area))
                  x-ticks y-ticks z-ticks x-min x-max y-min y-max z-min z-max)))

(define (mix3d* renderer1 renderer2)
  (match-define (renderer3d f1 x1-ticks y1-ticks z1-ticks
                            x1-min x1-max y1-min y1-max z1-min z1-max)
    renderer1)
  (match-define (renderer3d f2 x2-ticks y2-ticks z2-ticks
                            x2-min x2-max y2-min y2-max z2-min z2-max)
    renderer2)
  (renderer3d (λ (area) (f1 area) (f2 area))
              (λ (x-min x-max)
                (remove-duplicates
                 (append (x1-ticks x-min x-max) (x2-ticks x-min x-max))))
              (λ (y-min y-max)
                (remove-duplicates
                 (append (y1-ticks y-min y-max) (y2-ticks y-min y-max))))
              (λ (z-min z-max)
                (remove-duplicates
                 (append (z1-ticks z-min z-max) (z2-ticks z-min z-max))))
              (maybe-min x1-min x2-min) (maybe-max x1-max x2-max)
              (maybe-min y1-min y2-min) (maybe-max y1-max y2-max)
              (maybe-min z1-min z2-min) (maybe-max z1-max z2-max)))

(define (mix3d . renderers)
  (when (null? renderers)
    (raise-type-error 'mix3d "at least one renderer" renderers))
  (for/fold ([renderer   (car renderers)])
            ([renderers  (in-list (cdr renderers))])
    (mix3d* renderer renderers)))
