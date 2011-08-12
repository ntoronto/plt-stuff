#lang racket/base

(require racket/gui
         "../common/math.rkt"
         "../common/points.rkt"
         "../common/ticks.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define point3d-label (make-parameter 'circle))
(define point3d-color (make-parameter "black"))
(define point3d-size (make-parameter 6))
(define point3d-line-width (make-parameter 2/3))
(define point3d-alpha (make-parameter 1.0))

(define (points3d
         vs [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
         #:label [label (point3d-label)]
         #:color [color (point3d-color)]
         #:size [size (point3d-size)]
         #:line-width [line-width (point3d-line-width)]
         #:alpha [alpha (point3d-alpha)])
  (let* ([xs     (delay (map (λ (v) (vector-ref v 0)) vs))]
         [ys     (delay (map (λ (v) (vector-ref v 1)) vs))]
         [zs     (delay (map (λ (v) (vector-ref v 2)) vs))]
         [x-min  (if x-min x-min (reg-min* (force xs)))]
         [x-max  (if x-max x-max (reg-max* (force xs)))]
         [y-min  (if y-min y-min (reg-min* (force ys)))]
         [y-max  (if y-max y-max (reg-max* (force ys)))]
         [z-min  (if z-min z-min (reg-min* (force zs)))]
         [z-max  (if z-max z-max (reg-max* (force zs)))])
    (renderer3d (add-points vs label color size line-width alpha)
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                x-min x-max y-min y-max z-min z-max)))
