#lang racket/base

(require racket/class racket/match racket/promise
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

(define (lines3d vs
                 #:x-min [x-min #f] #:x-max [x-max #f]
                 #:y-min [y-min #f] #:y-max [y-max #f]
                 #:z-min [z-min #f] #:z-max [z-max #f]
                 #:color [color (line3d-color)]
                 #:width [width (line3d-width)]
                 #:style [style (line3d-style)]
                 #:alpha [alpha (line3d-alpha)])
  (let* ([xs     (delay (map (λ (v) (vector-ref v 0)) vs))]
         [ys     (delay (map (λ (v) (vector-ref v 1)) vs))]
         [zs     (delay (map (λ (v) (vector-ref v 2)) vs))]
         [x-min  (if x-min x-min (apply regular-min (force xs)))]
         [x-max  (if x-max x-max (apply regular-max (force xs)))]
         [y-min  (if y-min y-min (apply regular-min (force ys)))]
         [y-max  (if y-max y-max (apply regular-max (force ys)))]
         [z-min  (if z-min z-min (apply regular-min (force zs)))]
         [z-max  (if z-max z-max (apply regular-max (force zs)))])
    (make-renderer3d (λ (area)
                       (send area set-alpha alpha)
                       (send area set-pen color width style)
                       (send area add-lines vs))
                     (default-range->ticks (plot3d-tick-skip))
                     (default-range->ticks (plot3d-tick-skip))
                     (default-range->ticks (plot3d-tick-skip))
                     values
                     x-min x-max y-min y-max z-min z-max)))

(define (parametric3d fxyz t-min t-max
                      #:x-min [x-min #f] #:x-max [x-max #f]
                      #:y-min [y-min #f] #:y-max [y-max #f]
                      #:z-min [z-min #f] #:z-max [z-max #f]
                      #:samples [samples (line3d-samples)]
                      #:color [color (line3d-color)]
                      #:width [width (line3d-width)]
                      #:style [style (line3d-style)]
                      #:alpha [alpha (line3d-alpha)])
  (define ts (real-seq t-min t-max samples))
  (lines3d (match fxyz
             [(vector fx fy fz)  (map vector
                                      (map fx ts) (map fy ts) (map fz ts))]
             [(? procedure?)  (map fxyz ts)])
           #:x-min x-min #:x-max x-max
           #:y-min y-min #:y-max y-max
           #:z-min z-min #:z-max z-max
           #:color color #:width width #:style style #:alpha alpha))
