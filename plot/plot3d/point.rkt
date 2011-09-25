#lang racket/base

(require racket/class racket/list racket/match racket/contract
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/point.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "renderer.rkt"
         "ticks.rkt")

(provide (all-defined-out))

(defparam point3d-symbol point-symbol/c 'circle)
(defparam point3d-color plot-color/c 0)
(defparam point3d-size font-size/c 6)
(defparam point3d-line-width (real>=/c 0) 1)
(defparam point3d-alpha (real-in 0 1) 1)

;; ===================================================================================================

(define ((points3d-render-proc vs symbol color size line-width alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color line-width 'solid)
  (send area put-glyphs vs symbol size)
  
  (cond [label  (point-legend-entry label symbol color size line-width)]
        [else   empty]))

(defproc (points3d
          [vs  (listof (vector/c real? real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:symbol symbol point-symbol/c (point3d-symbol)]
          [#:color color plot-color/c (point3d-color)]
          [#:size size font-size/c (point3d-size)]
          [#:line-width line-width (real>=/c 0) (point3d-line-width)]
          [#:alpha alpha (real-in 0 1) (point3d-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (let ([vs  (filter vall-regular? vs)])
    (cond [(empty? vs)  null-renderer3d]
          [else (match-define (list (vector xs ys zs) ...) vs)
                (let ([x-min  (if x-min x-min (apply min* xs))]
                      [x-max  (if x-max x-max (apply max* xs))]
                      [y-min  (if y-min y-min (apply min* ys))]
                      [y-max  (if y-max y-max (apply max* ys))]
                      [z-min  (if z-min z-min (apply min* ys))]
                      [z-max  (if z-max z-max (apply max* ys))])
                  (make-renderer3d
                   (points3d-render-proc vs symbol color size line-width alpha label)
                   default-3d-ticks-fun null-3d-bounds-fun
                   x-min x-max y-min y-max z-min z-max))])))
