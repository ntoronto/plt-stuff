#lang racket/base

(require racket/class racket/match racket/list racket/contract
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "area.rkt"
         "renderer.rkt"
         "ticks.rkt"
         "sample.rkt")

(provide (all-defined-out))

(defparam line3d-samples (integer>=/c 2) 500)
(defparam line3d-color plot-color/c 1)
(defparam line3d-width (real>=/c 0) 1)
(defparam line3d-style line-style/c 'solid)
(defparam line3d-alpha (real-in 0 1) 1)

;; ===================================================================================================

(define ((lines3d-render-proc vs-fun color width style alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines (vs-fun))
  
  (cond [label  (line-legend-entry label color width style)]
        [else  empty]))

(define (make-lines3d-renderer
         vs-thnk x-min x-max y-min y-max z-min z-max color width style alpha label)
  (define rvs (filter vall-regular? (vs-thnk)))
  (cond [(empty? rvs)  null-renderer3d]
        [else  (match-define (list (vector rxs rys rzs) ...) rvs)
               (let ([x-min  (if x-min x-min (apply min* rxs))]
                     [x-max  (if x-max x-max (apply max* rxs))]
                     [y-min  (if y-min y-min (apply min* rys))]
                     [y-max  (if y-max y-max (apply max* rys))]
                     [z-min  (if z-min z-min (apply min* rzs))]
                     [z-max  (if z-max z-max (apply max* rzs))])
                 (make-renderer3d
                  (lines3d-render-proc vs-thnk color width style alpha label)
                  default-3d-ticks-fun
                  null-3d-bounds-fun
                  x-min x-max y-min y-max z-min z-max))]))

(defproc (lines3d
          [vs  (listof (vector/c real? real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:color color plot-color/c (line3d-color)]
          [#:width width (real>=/c 0) (line3d-width)]
          [#:style style line-style/c (line3d-style)]
          [#:alpha alpha (real-in 0 1) (line3d-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (make-lines3d-renderer (λ () vs) x-min x-max y-min y-max z-min z-max color width style alpha label))

(defproc (parametric3d
          [f (real? . -> . (vector/c real? real? real?))]
          [t-min real?] [t-max real?]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 2) (line3d-samples)]
          [#:color color plot-color/c (line3d-color)]
          [#:width width (real>=/c 0) (line3d-width)]
          [#:style style line-style/c (line3d-style)]
          [#:alpha alpha (real-in 0 1) (line3d-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (make-lines3d-renderer
   (λ () (sample-parametric f t-min t-max (samples/animating? samples)))
   x-min x-max y-min y-max z-min z-max color width style alpha label))
