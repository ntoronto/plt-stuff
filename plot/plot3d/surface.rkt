#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/marching-squares.rkt"
         "../common/ticks.rkt"
         "../common/draw.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "area.rkt"
         "renderer.rkt"
         "ticks.rkt"
         "sample.rkt")

(provide (all-defined-out))

(defparam surface3d-samples (integer>=/c 2) 41)
(defparam surface3d-color plot-color/c 0)
(defparam surface3d-style fill-style/c 'solid)
(defparam surface3d-line-color plot-color/c 0)
(defparam surface3d-line-width (real>=/c 0) 1/3)
(defparam surface3d-line-style line-style/c 'solid)
(defparam surface3d-alpha (real-in 0 1) 1)

;; ===================================================================================================
;; Surface plots of R R -> R functions

(define ((surface3d-render-proc f samples color style line-color line-width line-style alpha label)
         area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))
  (match-define (list xs ys zss) (f x-min x-max (samples/animating? samples)
                                    y-min y-max (samples/animating? samples)))
  
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (for ([ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [zs0  (in-vector zss)]
        [zs1  (in-vector zss 1)]
        #:when #t
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [z1  (in-vector zs0)]
        [z2  (in-vector zs0 1)]
        [z3  (in-vector zs1 1)]
        [z4  (in-vector zs1)])
    (send area put-polygon
          (list (vector xa ya z1) (vector xb ya z2) (vector xb yb z3) (vector xa yb z4))))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else   empty]))

(define ((surface3d-bounds-fun f samples) x-min x-max y-min y-max z-min z-max)
  (cond [(and x-min x-max y-min y-max)
         (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
         (define zs (filter regular? (2d-sample->list zss)))
         (if (empty? zs)
             (values x-min x-max y-min y-max z-min z-max)
             (values x-min x-max y-min y-max
                     (if z-min z-min (apply min* zs))
                     (if z-max z-max (apply max* zs))))]
        [else  (values x-min x-max y-min y-max z-min z-max)]))

(defproc (surface3d
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 2) (surface3d-samples)]
          [#:color color plot-color/c (surface3d-color)]
          [#:style style fill-style/c (surface3d-style)]
          [#:line-color line-color plot-color/c (surface3d-line-color)]
          [#:line-width line-width (real>=/c 0) (surface3d-line-width)]
          [#:line-style line-style line-style/c (surface3d-line-style)]
          [#:alpha alpha (real-in 0 1) (surface3d-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (make-renderer3d
   (surface3d-render-proc g samples color style line-color line-width line-style alpha label)
   default-3d-ticks-fun
   (surface3d-bounds-fun g samples)
   x-min x-max y-min y-max z-min z-max))
