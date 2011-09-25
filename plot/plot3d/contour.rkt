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
         "sample.rkt"
         "surface.rkt")

(provide (all-defined-out))

(defproc (default-contour3d-line-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->color/line 5) (->color/line 0) (->color/line 1))
              (length zs)))

(defproc (default-contour3d-fill-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->color/fill 5) (->color/fill 0) (->color/fill 1))
              (sub1 (length zs))))

(defparam contour3d-samples (integer>=/c 2) 41)
(defparam contour3d-levels (or/c (integer>=/c 1) 'auto) 'auto)
(defparam contour3d-colors plot-colors/c default-contour3d-line-colors)
(defparam contour3d-widths line-widths/c '(1))
(defparam contour3d-styles line-styles/c '(solid long-dash))
(defparam contour3d-alphas alphas/c '(1))

(defparam contour3d-interval-colors plot-colors/c default-contour3d-fill-colors)
(defparam contour3d-interval-line-colors plot-colors/c '(0))
(defparam contour3d-interval-line-widths line-widths/c '(1/3))
(defparam contour3d-interval-line-styles line-styles/c '(solid))
(defparam contour3d-interval-alphas alphas/c '(1))

;; ===================================================================================================
;; Contour lines in 3D (using marching squares)

(define ((contours3d-render-proc f levels samples colors widths styles alphas label) area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))
  (match-define (list xs ys zss) (f x-min x-max (samples/animating? samples)
                                    y-min y-max (samples/animating? samples)))
  (define zs (if (eq? levels 'auto)
                 (map tick-p (default-3d-z-ticks-fun z-min z-max))
                 (linear-seq z-min z-max levels #:start? #f #:stop? #f)))
  (define cs (apply-colors colors zs))
  (define ws (apply-line-widths widths zs))
  (define ss (apply-line-styles styles zs))
  (define as (apply-alphas alphas zs))
  
  (for ([z  (in-list zs)]
        [color  (in-cycle cs)]
        [width  (in-cycle ws)]
        [style  (in-cycle ss)]
        [alpha  (in-cycle as)])
    (send area put-alpha alpha)
    (send area put-pen color width style)
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
      (define lines (heights->lines (exact->inexact z)
                                    (exact->inexact z1) (exact->inexact z2)
                                    (exact->inexact z3) (exact->inexact z4)))
      (for ([line  (in-list lines)])
        (match-define (vector x1 y1 x2 y2) (scale-normalized-line line xa xb ya yb))
        (send area put-line
              (vector x1 y1 z) (vector x2 y2 z)
              (center-coord (list (vector xa ya z1) (vector xb ya z2)
                                  (vector xa yb z3) (vector xb yb z4)))))))
  
  (cond [label  (line-legend-entries label zs colors widths styles)]
        [else   empty]))

(defproc (contours3d
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:levels levels (integer>=/c 1) (contour3d-levels)]
          [#:samples samples (integer>=/c 2) (contour3d-samples)]
          [#:colors colors plot-colors/c (contour3d-colors)]
          [#:widths widths line-widths/c (contour3d-widths)]
          [#:styles styles line-styles/c (contour3d-styles)]
          [#:alphas alphas alphas/c (contour3d-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (make-renderer3d
   (contours3d-render-proc g levels samples colors widths styles alphas label)
   default-3d-ticks-fun
   (surface3d-bounds-fun g samples)
   x-min x-max y-min y-max z-min z-max))

;; ===================================================================================================
;; Contour intervals in 3D (using marching squares)

(define ((contour3d-intervals-render-proc
          f levels samples colors line-colors line-widths line-styles
          contour-colors contour-widths contour-styles alphas label)
         area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))  
  (match-define (list xs ys zss) (f x-min x-max (samples/animating? samples)
                                    y-min y-max (samples/animating? samples)))
  (define contour-zs (cond [(eq? levels 'auto)  (map tick-p (default-3d-z-ticks-fun z-min z-max))]
                           [else  (linear-seq z-min z-max levels #:start? #f #:end? #f)]))
  
  (define zs (append (list z-min) contour-zs (list z-max)))
  (define cs (apply-colors colors zs))
  (define lcs (apply-colors line-colors zs))
  (define lws (apply-line-widths line-widths zs))
  (define lss (apply-line-styles line-styles zs))
  (define as (apply-alphas alphas zs))
  
  (for ([za  (in-list zs)]
        [zb  (in-list (rest zs))]
        [color  (in-cycle cs)]
        [line-color  (in-cycle lcs)]
        [line-width  (in-cycle lws)]
        [line-style  (in-cycle lss)]
        [alpha  (in-cycle as)])
    (send area put-alpha alpha)
    (send area put-pen line-color line-width line-style)
    (send area put-brush color 'solid)
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
      (for ([poly  (in-list (heights->mid-polys (exact->inexact za) (exact->inexact zb)
                                                (exact->inexact z1) (exact->inexact z2)
                                                (exact->inexact z3) (exact->inexact z4)))])
        (define rect (list (vector xa ya z1) (vector xb ya z2) (vector xb yb z3) (vector xa yb z4)))
        (send area put-polygon
              (cond [(equal? poly 'full)  rect]
                    [else  (scale-normalized-poly poly xa xb ya yb)])
              (center-coord rect)))))
  
  ((contours3d-render-proc f levels samples contour-colors contour-widths contour-styles alphas #f)
   area)
  
  (cond [label  (contour-intervals-legend-entries
                 label z-min z-max contour-zs colors '(solid) line-colors line-widths line-styles
                 contour-colors contour-widths contour-styles)]
        [else  empty]))

(defproc (contour3d-intervals
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:levels levels (integer>=/c 1) (contour3d-levels)]
          [#:samples samples (integer>=/c 2) (contour3d-samples)]
          [#:colors colors plot-colors/c (contour3d-interval-colors)]
          [#:line-colors line-colors plot-colors/c (contour3d-interval-line-colors)]
          [#:line-widths line-widths line-widths/c (contour3d-interval-line-widths)]
          [#:line-styles line-styles line-styles/c (contour3d-interval-line-styles)]
          [#:contour-colors contour-colors plot-colors/c (contour3d-colors)]
          [#:contour-widths contour-widths line-widths/c (contour3d-widths)]
          [#:contour-styles contour-styles line-styles/c (contour3d-styles)]
          [#:alphas alphas alphas/c (contour3d-interval-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (make-renderer3d
   (contour3d-intervals-render-proc
    g levels samples colors line-colors line-widths line-styles
    contour-colors contour-widths contour-styles alphas label)
   default-3d-ticks-fun
   (surface3d-bounds-fun g samples)
   x-min x-max y-min y-max z-min z-max))
