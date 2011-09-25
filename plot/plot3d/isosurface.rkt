#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract
         "../common/marching-cubes.rkt"
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt"
         "../common/draw.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "area.rkt"
         "renderer.rkt"
         "sample.rkt"
         "ticks.rkt")

(provide (all-defined-out))

(defproc (default-isosurfaces3d-colors [ds  (listof real?)]) (listof plot-color/c)
  (color-seq* '((0 0 128) (128 0 0) (255 248 192)) (length ds)))

(defparam isosurface3d-samples (integer>=/c 2) 41)
(defparam isosurface3d-color plot-color/c 0)
(defparam isosurface3d-line-color plot-color/c 0)
(defparam isosurface3d-line-width (real>=/c 0) 1/3)
(defparam isosurface3d-line-style line-style/c 'solid)
(defparam isosurface3d-alpha (real-in 0 1) 1)

(defparam isosurfaces3d-levels (integer>=/c 1) 3)
(defparam isosurfaces3d-colors plot-colors/c default-isosurfaces3d-colors)
(defparam isosurfaces3d-line-colors plot-colors/c default-isosurfaces3d-colors)
(defparam isosurfaces3d-line-widths line-widths/c '(1/3))
(defparam isosurfaces3d-line-styles line-styles/c '(transparent))
(defparam isosurfaces3d-alphas alphas/c '(1/2))

(defparam polar3d-samples (integer>=/c 2) 41)
(defparam polar3d-color plot-color/c 0)
(defparam polar3d-line-color plot-color/c 0)
(defparam polar3d-line-width (real>=/c 0) 1/3)
(defparam polar3d-line-style line-style/c 'solid)
(defparam polar3d-alpha (real-in 0 1) 1)

;; ===================================================================================================
;; Surfaces of constant value (isosurfaces)

(define (scale-normalized-polys polys xa xb ya yb za zb)
  (map (λ (poly) (scale-normalized-poly poly xa xb ya yb za zb))
       polys))

(define ((isosurface3d-render-proc f d samples color line-color line-width line-style alpha label)
         area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define z-min (send area get-z-min))
  (define z-max (send area get-z-max))
  
  (match-define (list xs ys zs dsss)
    (f x-min x-max (samples/animating? samples)
       y-min y-max (samples/animating? samples)
       z-min z-max (samples/animating? samples)))
  
  (send area put-alpha alpha)
  (send area put-brush color 'solid)
  (send area put-pen line-color line-width line-style)
  (for ([za  (in-list zs)]
        [zb  (in-list (rest zs))]
        [dss0  (in-vector dsss)]
        [dss1  (in-vector dsss 1)]
        #:when #t
        [ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [ds00  (in-vector dss0)]
        [ds01  (in-vector dss0 1)]
        [ds10  (in-vector dss1)]
        [ds11  (in-vector dss1 1)]
        #:when #t
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [d1  (in-vector ds00)]
        [d2  (in-vector ds00 1)]
        [d3  (in-vector ds01 1)]
        [d4  (in-vector ds01)]
        [d5  (in-vector ds10)]
        [d6  (in-vector ds10 1)]
        [d7  (in-vector ds11 1)]
        [d8  (in-vector ds11)])
    (define polys
      (heights->cube-polys
       (exact->inexact d)
       (exact->inexact d1) (exact->inexact d2) (exact->inexact d3) (exact->inexact d4)
       (exact->inexact d5) (exact->inexact d6) (exact->inexact d7) (exact->inexact d8)))
    
    (when (not (empty? polys))
      (send area put-polygons
            (scale-normalized-polys polys xa xb ya yb za zb)
            (center-coord (list (vector xa ya za)
                                (vector xb yb zb))))))
  
  (cond [label  (rectangle-legend-entry
                 label color 'solid line-color line-width line-style)]
        [else  empty]))

(defproc (isosurface3d [f (real? real? real? . -> . real?)] [d real?]
                       [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
                       [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
                       [z-min (or/c real? #f) #f] [z-max (or/c real? #f) #f]
                       [#:samples samples (integer>=/c 2) (isosurface3d-samples)]
                       [#:color color plot-color/c (isosurface3d-color)]
                       [#:line-color line-color plot-color/c (isosurface3d-line-color)]
                       [#:line-width line-width (real>=/c 0) (isosurface3d-line-width)]
                       [#:line-style line-style line-style/c (isosurface3d-line-style)]
                       [#:alpha alpha (real-in 0 1) (isosurface3d-alpha)]
                       [#:label label (or/c string? #f) #f]
                       ) renderer3d?
  (define g (3d-function->sampler f))
  (make-renderer3d
   (isosurface3d-render-proc g d samples color
                             line-color line-width line-style alpha
                             label)
   default-3d-ticks-fun
   null-3d-bounds-fun
   x-min x-max y-min y-max z-min z-max))

;; ===================================================================================================
;; Nested isosurfaces

(define ((isosurfaces3d-render-proc
          f rd-min rd-max levels samples colors line-colors line-widths line-styles alphas label)
         area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define z-min (send area get-z-min))
  (define z-max (send area get-z-max))
  
  (match-define (list xs ys zs dsss)
    (f x-min x-max (samples/animating? samples)
       y-min y-max (samples/animating? samples)
       z-min z-max (samples/animating? samples)))
  
  (define-values (fd-min fd-max)
    (let ([regular-ds  (filter regular? (3d-sample->list dsss))])
      (values (if (empty? regular-ds) #f (apply min* regular-ds))
              (if (empty? regular-ds) #f (apply max* regular-ds)))))
  
  (define d-min (if rd-min rd-min fd-min))
  (define d-max (if rd-max rd-max fd-max))
  
  (cond
    [(not (and d-min d-max))  empty]
    [else
     (define ds (linear-seq d-min d-max levels #:start? rd-min #:end? rd-max))
     
     (for ([d           (in-list ds)]
           [color       (in-cycle (apply-colors colors ds))]
           [line-color  (in-cycle (apply-colors line-colors ds))]
           [line-width  (in-cycle (apply-line-widths line-widths ds))]
           [line-style  (in-cycle (apply-line-styles line-styles ds))]
           [alpha       (in-cycle (apply-alphas alphas ds))])
       (send area put-alpha alpha)
       (send area put-brush color 'solid)
       (send area put-pen line-color line-width line-style)
       (for ([za  (in-list zs)]
             [zb  (in-list (rest zs))]
             [dss0  (in-vector dsss)]
             [dss1  (in-vector dsss 1)]
             #:when #t
             [ya  (in-list ys)]
             [yb  (in-list (rest ys))]
             [ds00  (in-vector dss0)]
             [ds01  (in-vector dss0 1)]
             [ds10  (in-vector dss1)]
             [ds11  (in-vector dss1 1)]
             #:when #t
             [xa  (in-list xs)]
             [xb  (in-list (rest xs))]
             [d1  (in-vector ds00)]
             [d2  (in-vector ds00 1)]
             [d3  (in-vector ds01 1)]
             [d4  (in-vector ds01)]
             [d5  (in-vector ds10)]
             [d6  (in-vector ds10 1)]
             [d7  (in-vector ds11 1)]
             [d8  (in-vector ds11)])
         (define polys
           (heights->cube-polys
            (exact->inexact d)
            (exact->inexact d1) (exact->inexact d2) (exact->inexact d3) (exact->inexact d4)
            (exact->inexact d5) (exact->inexact d6) (exact->inexact d7) (exact->inexact d8)))
         
         (when (not (empty? polys))
           (send area put-polygons
                 (scale-normalized-polys polys xa xb ya yb za zb)
                 (center-coord (list (vector xa ya za) (vector xb yb zb)))))))
     
     (cond
       [label  (rectangle-legend-entries
                label ds colors '(solid) line-colors line-widths line-styles)]
       [else  empty])]))

(defproc (isosurfaces3d [f (real? real? real? . -> . real?)]
                        [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
                        [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
                        [z-min (or/c real? #f) #f] [z-max (or/c real? #f) #f]
                        [#:d-min d-min (or/c real? #f) #f] [#:d-max d-max (or/c real? #f) #f]
                        [#:levels levels (integer>=/c 1) (isosurfaces3d-levels)]
                        [#:samples samples (integer>=/c 2) (isosurface3d-samples)]
                        [#:colors colors plot-colors/c (isosurfaces3d-colors)]
                        [#:line-colors line-colors plot-colors/c (isosurfaces3d-line-colors)]
                        [#:line-widths line-widths line-widths/c (isosurfaces3d-line-widths)]
                        [#:line-styles line-styles line-styles/c (isosurfaces3d-line-styles)]
                        [#:alphas alphas alphas/c (isosurfaces3d-alphas)]
                        [#:label label (or/c string? #f) #f]
                        ) renderer3d?
  (define g (3d-function->sampler f))
  (make-renderer3d
   (isosurfaces3d-render-proc g d-min d-max levels samples colors
                              line-colors line-widths line-styles alphas
                              label)
   default-3d-ticks-fun
   null-3d-bounds-fun
   x-min x-max y-min y-max z-min z-max))

;; ===================================================================================================

(define ((polar3d-render-proc f g samples color line-color line-width line-style alpha label)
         area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define z-min (send area get-z-min))
  (define z-max (send area get-z-max))
  
  (match-define (list xs ys zs dsss)
    (g x-min x-max (samples/animating? samples)
       y-min y-max (samples/animating? samples)
       z-min z-max (samples/animating? samples)))
  
  (send area put-alpha alpha)
  (send area put-brush color 'solid)
  (send area put-pen line-color line-width line-style)
  (for ([za  (in-list zs)]
        [zb  (in-list (rest zs))]
        [dss0  (in-vector dsss)]
        [dss1  (in-vector dsss 1)]
        #:when #t
        [ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [ds00  (in-vector dss0)]
        [ds01  (in-vector dss0 1)]
        [ds10  (in-vector dss1)]
        [ds11  (in-vector dss1 1)]
        #:when #t
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [d1  (in-vector ds00)]
        [d2  (in-vector ds00 1)]
        [d3  (in-vector ds01 1)]
        [d4  (in-vector ds01)]
        [d5  (in-vector ds10)]
        [d6  (in-vector ds10 1)]
        [d7  (in-vector ds11 1)]
        [d8  (in-vector ds11)])
    (define (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8)
      (define polys
        (heights->cube-polys
         0.0
         (exact->inexact d1) (exact->inexact d2) (exact->inexact d3) (exact->inexact d4)
         (exact->inexact d5) (exact->inexact d6) (exact->inexact d7) (exact->inexact d8)))
      (when (not (empty? polys))
        (send area put-polygons
              (scale-normalized-polys polys xa xb ya yb za zb)
              (center-coord (list (vector xa ya za)
                                  (vector xb yb zb))))))
    (cond [(and (xb . > . 0) (ya . < . 0) (yb . > . 0))
           (let* ([yb  -0.00001]
                  [d3  (f xb yb za)]
                  [d4  (f xa yb za)]
                  [d7  (f xb yb zb)]
                  [d8  (f xa yb zb)])
             (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8))
           (let* ([ya  0.00001]
                  [d1  (f xa ya za)]
                  [d2  (f xb ya za)]
                  [d5  (f xa ya zb)]
                  [d6  (f xb ya zb)])
             (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8))]
          [else
           (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8)]))
  
  (cond [label  (rectangle-legend-entry
                 label color 'solid line-color line-width line-style)]
        [else  empty]))

(defproc (polar3d [f (real? real? . -> . real?)]
                  [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                  [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                  [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
                  [#:samples samples (integer>=/c 2) (polar3d-samples)]
                  [#:color color plot-color/c (polar3d-color)]
                  [#:line-color line-color plot-color/c (polar3d-line-color)]
                  [#:line-width line-width (real>=/c 0) (polar3d-line-width)]
                  [#:line-style line-style line-style/c (polar3d-line-style)]
                  [#:alpha alpha (real-in 0 1) (polar3d-alpha)]
                  [#:label label (or/c string? #f) #f]
                  ) renderer3d?
  (define rvs (filter vall-regular? (sample-3d-polar f 0 2pi (* 2 samples) -1/2pi 1/2pi samples)))
  (cond [(empty? rvs)  null-renderer3d]
        [else
         (match-define (list (vector rxs rys rzs) ...) rvs)
         (let ([x-min  (if x-min x-min (apply min* rxs))]
               [x-max  (if x-max x-max (apply max* rxs))]
               [y-min  (if y-min y-min (apply min* rys))]
               [y-max  (if y-max y-max (apply max* rys))]
               [z-min  (if z-min z-min (apply min* rzs))]
               [z-max  (if z-max z-max (apply max* rzs))])
           (define new-f (3d-polar->3d-function f))
           (define g (3d-function->sampler new-f))
           (make-renderer3d
            (polar3d-render-proc new-f g samples color line-color line-width line-style alpha label)
            default-3d-ticks-fun
            null-3d-bounds-fun
            x-min x-max y-min y-max z-min z-max))]))
