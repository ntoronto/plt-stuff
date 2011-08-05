#lang racket/base

(require racket/match
         "../common/utils.rkt")

(provide (all-defined-out))

(struct renderer2d (f x-min x-max y-min y-max) #:transparent)

(define (mix* renderer1 renderer2)
  (match-define (renderer2d f1 x1-min x1-max y1-min y1-max) renderer1)
  (match-define (renderer2d f2 x2-min x2-max y2-min y2-max) renderer2)
  (renderer2d (λ (area) (f1 area) (f2 area))
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
