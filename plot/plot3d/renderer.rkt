#lang racket/base

(require racket/match
         "../common/utils.rkt")

(provide (all-defined-out))

(struct renderer3d (f x-min x-max y-min y-max z-min z-max) #:transparent)

(define (mix3d* renderer1 renderer2)
  (match-define (renderer3d f1 x1-min x1-max y1-min y1-max z1-min z1-max)
    renderer1)
  (match-define (renderer3d f2 x2-min x2-max y2-min y2-max z2-min z2-max)
    renderer2)
  (renderer3d (λ (area) (f1 area) (f2 area))
              (maybe-min x1-min x2-min) (maybe-max x1-max x2-max)
              (maybe-min y1-min y2-min) (maybe-max y1-max y2-max)
              (maybe-min z1-min z2-min) (maybe-max z1-max z2-max)))

(define (mix3d . renderers)
  (when (null? renderers)
    (raise-type-error 'mix3d "at least one renderer" renderers))
  (for/fold ([renderer   (car renderers)])
            ([renderers  (in-list (cdr renderers))])
    (mix3d* renderer renderers)))
