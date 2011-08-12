#lang racket/base

(require racket/gui
         "../common/math.rkt"
         "../common/ticks.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define line3d-samples (make-parameter 500))
(define line3d-color (make-parameter "darkred"))
(define line3d-width (make-parameter 1))
(define line3d-style (make-parameter 'solid))
(define line3d-alpha (make-parameter 1.0))

(define (line3d
         vs [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
         #:color [color (line3d-color)]
         #:width [width (line3d-width)]
         #:style [style (line3d-style)]
         #:alpha [alpha (line3d-alpha)])
  (let* ([xs     (delay (map (λ (v) (vector-ref v 0)) vs))]
         [ys     (delay (map (λ (v) (vector-ref v 1)) vs))]
         [zs     (delay (map (λ (v) (vector-ref v 2)) vs))]
         [x-min  (if x-min x-min (reg-min* (force xs)))]
         [x-max  (if x-max x-max (reg-max* (force xs)))]
         [y-min  (if y-min y-min (reg-min* (force ys)))]
         [y-max  (if y-max y-max (reg-max* (force ys)))]
         [z-min  (if z-min z-min (reg-min* (force zs)))]
         [z-max  (if z-max z-max (reg-max* (force zs)))])
    (renderer3d (λ (area)
                  (send area set-alpha alpha)
                  (send area set-pen color width style)
                  (send area add-lines vs))
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                x-min x-max y-min y-max z-min z-max)))

(define (parametric3d
         fxyz t-min t-max
         [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
                    #:color [color (line3d-color)]
                    #:width [width (line3d-width)]
                    #:style [style (line3d-style)]
                    #:alpha [alpha (line3d-alpha)])
  (define ts (real-seq t-min t-max (line3d-samples)))
  (line3d (match fxyz
            [(vector fx fy fz)  (map vector
                                     (map fx ts) (map fy ts) (map fz ts))]
            [(? procedure?)  (map fxyz ts)])
          x-min x-max y-min y-max z-min z-max
          #:color color #:width width #:style style #:alpha alpha))
