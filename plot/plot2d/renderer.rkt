#lang racket/base

(require racket/match racket/gui
         "../common/math.rkt")

(provide (all-defined-out))

(struct renderer2d (f x-ticks y-ticks x-min x-max y-min y-max) #:transparent
        #:guard (λ (f x-ticks y-ticks x-min x-max y-min y-max _)
                  (values (λ (area)
                            (send area clip-to-bounds x-min x-max y-min y-max)
                            (f area))
                          x-ticks y-ticks x-min x-max y-min y-max)))

(define (mix* renderer1 renderer2)
  (match-define (renderer2d f1 x1-ticks y1-ticks x1-min x1-max y1-min y1-max)
    renderer1)
  (match-define (renderer2d f2 x2-ticks y2-ticks x2-min x2-max y2-min y2-max)
    renderer2)
  (renderer2d (λ (area) (f1 area) (f2 area))
              (λ (x-min x-max)
                (remove-duplicates
                 (append (x1-ticks x-min x-max) (x2-ticks x-min x-max))))
              (λ (y-min y-max)
                (remove-duplicates
                 (append (y1-ticks y-min y-max) (y2-ticks y-min y-max))))
              (maybe-min x1-min x2-min)
              (maybe-max x1-max x2-max)
              (maybe-min y1-min y2-min)
              (maybe-max y1-max y2-max)))

(define (mix . renderers)
  (when (null? renderers)
    (raise-type-error 'mix2d "at least one renderer" renderers))
  (for/fold ([renderer  (car renderers)])
            ([renderers  (in-list (cdr renderers))])
    (mix* renderer renderers)))
