#lang racket/base

;; Renderers for contour lines and contour intervals

(require racket/contract racket/class racket/match racket/list racket/flonum
         "../common/math.rkt"
         "../common/draw.rkt"
         "../common/marching-squares.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "renderer.rkt"
         "ticks.rkt"
         "sample.rkt")

(provide (all-defined-out))

(defproc (default-contour-line-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->color/line 5) (->color/line 0) (->color/line 1))
              (length zs)))

(defproc (default-contour-fill-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->color/fill 5) (->color/fill 0) (->color/fill 1))
              (sub1 (length zs))))

(defparam contour-samples (integer>=/c 2) 51)
(defparam contour-levels (integer>=/c 1) 6)
(defparam contour-colors plot-colors/c default-contour-line-colors)
(defparam contour-widths line-widths/c '(1))
(defparam contour-styles line-styles/c '(solid long-dash))
(defparam contour-alphas alphas/c '(1))

(defparam contour-interval-colors plot-colors/c default-contour-fill-colors)
(defparam contour-interval-styles fill-styles/c '(solid))
(defparam contour-interval-alphas alphas/c '(1))

;; ===================================================================================================
;; Contour lines

(define ((contours-render-proc f levels samples colors widths styles alphas label) area)
  (let/ec return
    (define-values (x-min x-max y-min y-max) (send area get-bounds))
    (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
    
    (define-values (z-min z-max)
      (let ([zs  (filter regular? (2d-sample->list zss))])
        (when (empty? zs) (return empty))
        (values (apply min* zs) (apply max* zs))))
    
    (define zs (linear-seq z-min z-max levels #:start? #f #:end? #f))
    (define cs (apply-colors colors zs))
    (define ws (apply-line-widths widths zs))
    (define ss (apply-line-styles styles zs))
    (define as (apply-alphas alphas zs))
    
    (for ([z      (in-list zs)]
          [color  (in-cycle cs)]
          [width  (in-cycle ws)]
          [style  (in-cycle ss)]
          [alpha  (in-cycle as)])
      (send area set-alpha alpha)
      (send area set-pen color width style)
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
        (for/list ([line  (in-list (heights->lines (exact->inexact z)
                                                   (exact->inexact z1) (exact->inexact z2)
                                                   (exact->inexact z3) (exact->inexact z4)))])
          (match-define (vector x1 y1 x2 y2) (scale-normalized-line line xa xb ya yb))
          (send area put-line (vector x1 y1) (vector x2 y2)))))
    
    (cond [label  (line-legend-entries label zs colors widths styles)]
          [else   empty])))

(defproc (contours
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:levels levels (integer>=/c 1) (contour-levels)]
          [#:samples samples (integer>=/c 2) (contour-samples)]
          [#:colors colors plot-colors/c (contour-colors)]
          [#:widths widths line-widths/c (contour-widths)]
          [#:styles styles line-styles/c (contour-styles)]
          [#:alphas alphas alphas/c (contour-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g (2d-function->sampler f))
  (make-renderer2d
   (contours-render-proc g levels samples colors widths styles alphas label)
   default-2d-ticks-fun
   null-2d-bounds-fun
   x-min x-max y-min y-max))

;; ===================================================================================================
;; Contour intervals

(define (take-xy v)
  (match-define (vector x y z) v)
  (vector x y))

(define ((contour-intervals-render-proc
          f levels samples colors styles contour-colors contour-widths contour-styles alphas label)
         area)
  (let/ec return
    (define-values (x-min x-max y-min y-max) (send area get-bounds))
    (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
    
    (define-values (z-min z-max)
      (let ([flat-zs  (filter regular? (2d-sample->list zss))])
        (when (empty? flat-zs) (return empty))
        (values (apply min* flat-zs) (apply max* flat-zs))))
    (define contour-zs (linear-seq z-min z-max levels #:start? #f #:end? #f))
    
    (define zs (append (list z-min) contour-zs (list z-max)))
    (define cs (map ->color/fill (apply-colors colors zs)))
    (define fss (map ->style/fill (apply-fill-styles styles zs)))
    (define pss (map (λ (fill-style) (if (eq? fill-style 'solid) 'solid 'transparent)) fss))
    (define as (apply-alphas alphas zs))
    
    (for ([za     (in-list zs)]
          [zb     (in-list (rest zs))]
          [color  (in-cycle cs)]
          [fill-style       (in-cycle fss)]
          [poly-line-style  (in-cycle pss)]
          [alpha  (in-cycle as)])
      (define polys
        (append*
         (for/list ([ya  (in-list ys)]
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
           (for/list ([poly  (in-list (heights->mid-polys (exact->inexact za) (exact->inexact zb)
                                                          (exact->inexact z1) (exact->inexact z2)
                                                          (exact->inexact z3) (exact->inexact z4)))])
             (cond [(equal? poly 'full)  (list (vector xa ya) (vector xa yb)
                                               (vector xb yb) (vector xb ya))]
                   [else  (map take-xy (scale-normalized-poly poly xa xb ya yb))])))))
      
      (define (draw-polys)
        (for ([poly  (in-list polys)])
          (send area put-polygon poly)))
      
      (cond [(= alpha 1)
             (send area set-pen color 1 poly-line-style)
             (send area set-brush color fill-style)
             (send area set-alpha 1)
             (draw-polys)]
            [else
             ;; draw the outlines with reduced alpha first
             (send area set-pen color 1 poly-line-style)
             (send area set-brush color 'transparent)
             (send area set-alpha (alpha-expt alpha 1/8))
             (draw-polys)
             ;; now draw the centers
             (send area set-pen color 1 'transparent)
             (send area set-brush color fill-style)
             (send area set-alpha alpha)
             (draw-polys)]))
    
    ((contours-render-proc f levels samples contour-colors contour-widths contour-styles alphas #f)
     area)
    
    (cond [label  (contour-intervals-legend-entries
                   label z-min z-max contour-zs
                   cs fss cs '(1) pss contour-colors contour-widths contour-styles)]
          [else   empty])))

(defproc (contour-intervals
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:levels levels (integer>=/c 1) (contour-levels)]
          [#:samples samples (integer>=/c 2) (contour-samples)]
          [#:colors colors plot-colors/c (contour-interval-colors)]
          [#:styles styles fill-styles/c (contour-interval-styles)]
          [#:contour-colors contour-colors plot-colors/c (contour-colors)]
          [#:contour-widths contour-widths line-widths/c (contour-widths)]
          [#:contour-styles contour-styles line-styles/c (contour-styles)]
          [#:alphas alphas alphas/c (contour-interval-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g (2d-function->sampler f))
  (make-renderer2d
   (contour-intervals-render-proc g levels samples colors styles
                                  contour-colors contour-widths contour-styles
                                  alphas label)
   default-2d-ticks-fun
   null-2d-bounds-fun
   x-min x-max y-min y-max))
