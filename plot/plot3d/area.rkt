#lang racket/base

(require racket/class racket/match racket/list racket/math racket/contract
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/draw.rkt"
         "../common/contract.rkt"
         "../common/transform.rkt"
         "transform.rkt"
         "shape.rkt"
         "clip.rkt"
         "sample.rkt")

(provide (all-defined-out))

(defparam plot3d-samples (integer>=/c 2) 41)

(defparam plot3d-animating? boolean? #f)

(defparam plot3d-angle real? 30)
(defparam plot3d-altitude real? 60)

(defparam plot3d-ambient-light-value (real-in 0 1) 2/3)
(defparam plot3d-diffuse-light? boolean? #t)
(defparam plot3d-specular-light? boolean? #t)

(defparam plot3d-tick-size (real>=/c 0) 10)

(defparam plot3d-title (or/c string? #f) #f)
(defparam plot3d-x-label (or/c string? #f) "x axis")
(defparam plot3d-y-label (or/c string? #f) "y axis")
(defparam plot3d-z-label (or/c string? #f) #f)

(defproc (samples/animating? [samples (integer>=/c 2)]) (integer>=/c 2)
  (cond [(plot3d-animating?)  (max 2 (ceiling (* 1/4 samples)))]
        [else  samples]))

;; ===================================================================================================
;; Plot area

(define 3d-plot-area%
  (class plot-area%
    (init-field x-ticks y-ticks z-ticks x-min x-max y-min y-max z-min z-max)
    (init dc)
    (inherit set-alpha set-pen set-brush set-background set-font set-text-foreground
             get-text-width get-text-extent get-char-height get-char-baseline get-size
             set-clipping-rect clear-clipping-rect reset-drawing-params
             clear draw-polygon draw-rectangle draw-line draw-lines draw-text
             draw-glyphs draw-arrow-glyph draw-tick draw-legend)
    
    (super-make-object dc)
    
    (reset-drawing-params)
    
    (define char-height (get-char-height))
    
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    (define clip-z-min z-min)
    (define clip-z-max z-max)
    
    (define/public (clip-to-bounds rx-min rx-max ry-min ry-max rz-min rz-max)
      (set! clipping? #t)
      (define cx-min (if rx-min (max* x-min rx-min) x-min))
      (define cx-max (if rx-max (min* x-max rx-max) x-max))
      (define cy-min (if ry-min (max* y-min ry-min) y-min))
      (define cy-max (if ry-max (min* y-max ry-max) y-max))
      (define cz-min (if rz-min (max* z-min rz-min) z-min))
      (define cz-max (if rz-max (min* z-max rz-max) z-max))
      (let ([cx-min  (min* cx-min cx-max)]
            [cx-max  (max* cx-min cx-max)]
            [cy-min  (min* cy-min cy-max)]
            [cy-max  (max* cy-min cy-max)]
            [cz-min  (min* cz-min cz-max)]
            [cz-max  (max* cz-min cz-max)])
        (set! clip-x-min cx-min)
        (set! clip-x-max cx-max)
        (set! clip-y-min cy-min)
        (set! clip-y-max cy-max)
        (set! clip-z-min cz-min)
        (set! clip-z-max cz-max)))
    
    (define (clip-to-none) (set! clipping? #f))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max clip-y-min clip-y-max clip-z-min clip-z-max)))
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-z-min) z-min)
    (define/public (get-z-max) z-max)
    (define/public (get-bounds) (values x-min x-max y-min y-max z-min z-max))
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define z-size (- z-max z-min))
    
    (define x-mid (* 1/2 (+ x-min x-max)))
    (define y-mid (* 1/2 (+ y-min y-max)))
    (define z-mid (* 1/2 (+ z-min z-max)))
    
    (define angle (plot3d-angle))
    (define altitude (plot3d-altitude))
    ;; FLOATING-POINT HACK: Adding an epsilon to the angle ensures that, when it is 90, 180
    ;; or 270, the x/y/z tick labels are drawn on the left side.
    (define theta (+ (degrees->radians angle) 0.00001))
    (define rho (degrees->radians altitude))
    
    (define do-axis-transforms? #f)
    
    (define identity-axis-transforms?
      (and (equal? (plot3d-x-transform) id-transform)
           (equal? (plot3d-y-transform) id-transform)
           (equal? (plot3d-z-transform) id-transform)))
    
    (define center
      (cond [identity-axis-transforms?
             (λ (v)
               (match-define (vector x y z) v)
               (vector (- x x-mid) (- y y-mid) (- z z-mid)))]
            [else
             (match-define (invertible-fun fx _) ((plot3d-x-transform) x-min x-max))
             (match-define (invertible-fun fy _) ((plot3d-y-transform) y-min y-max))
             (match-define (invertible-fun fz _) ((plot3d-z-transform) z-min z-max))
             (λ (v)
               (match-define (vector x y z) v)
               (if do-axis-transforms?
                   (vector (- (fx x) x-mid) (- (fy y) y-mid) (- (fz z) z-mid))
                   (vector (- x x-mid) (- y y-mid) (- z z-mid))))]))
    
    (define transform-matrix/no-rho
      (m3* (m3-rotate-z theta) (m3-scale (/ x-size) (/ y-size) (/ z-size))))
    (define transform-matrix (m3* (m3-rotate-x rho) transform-matrix/no-rho))
    
    (define (plot->view v) (m3-apply transform-matrix (center v)))
    (define (plot->view/no-rho v) (m3-apply transform-matrix/no-rho (center v)))
    (define (rotate/rho v) (m3-apply (m3-rotate-x rho) v))
    
    (define invtransform-matrix (m3-transpose transform-matrix))
    (define plot-right-dir (m3-apply invtransform-matrix (vector 1 0 0)))
    (define plot-up-dir (m3-apply invtransform-matrix (vector 0 0 1)))
    
    (define-values (dc-x-size dc-y-size) (get-size))
    
    (define view->dc* #f)
    (define (plot->dc v) (view->dc* (plot->view v)))
    
    (define (plot-dir->dc-angle v)
      (match-define (vector dx dy)
        (v- (plot->dc (v+ v (vector x-mid y-mid z-mid)))
            (plot->dc (vector x-mid y-mid z-mid))))
      (- (atan (- dy) dx)))
    
    (define/public (view->dc v) (view->dc* v))
    
    ;; Initial plot area margins leave enough room for the ticks
    (define init-left-margin (* 1/2 (plot3d-tick-size)))
    (define init-right-margin (* 1/2 (plot3d-tick-size)))
    (define init-top-margin (if (plot3d-title) (* 3/2 (get-char-height)) 0))
    (define init-bottom-margin (* 1/2 (plot3d-tick-size)))
    
    (define (make-view->dc area-x-min right area-y-min bottom)
      (define corners (list (vector x-min y-min z-min) (vector x-min y-min z-max)
                            (vector x-min y-max z-min) (vector x-min y-max z-max)
                            (vector x-max y-min z-min) (vector x-max y-min z-max)
                            (vector x-max y-max z-min) (vector x-max y-max z-max)))
      (match-define (list (vector xs ys zs) ...) (map plot->view corners))
      (define view-x-min (apply min xs))
      (define view-x-max (apply max xs))
      (define view-y-min (apply min ys))
      (define view-y-max (apply max ys))
      (define view-z-min (apply min zs))
      (define view-z-max (apply max zs))
      
      (define area-x-max (- dc-x-size right))
      (define area-y-max (- dc-y-size bottom))
      (define area-x-mid (* 1/2 (+ area-x-min area-x-max)))
      (define area-x-size (- area-x-max area-x-min))
      (define area-y-mid (* 1/2 (+ area-y-min area-y-max)))
      (define area-y-size (- area-y-max area-y-min))
      
      (define area-to-view-x (/ area-x-size (- view-x-max view-x-min)))
      (define area-to-view-z (/ area-y-size (- view-z-max view-z-min)))
      (λ (v)
        (match-define (vector x y z) v)
        (let ([x  (* x area-to-view-x)] [z  (* z area-to-view-z)])
          (vector (+ area-x-mid x) (- area-y-mid z)))))
    
    (set! view->dc* (make-view->dc init-left-margin init-right-margin
                                   init-top-margin init-bottom-margin))
    
    ;; Label drawing constants
    
    (define x-labels-y-min? ((cos theta) . >= . 0))
    (define y-labels-x-min? ((sin theta) . >= . 0))
    
    (define max-x-tick-label-width
      (apply max (map (λ (t) (get-text-width (tick-label t))) x-ticks)))
    
    (define max-y-tick-label-width
      (apply max (map (λ (t) (get-text-width (tick-label t))) y-ticks)))
    
    ;; Label drawing parameters
    
    (define (get-x-label-params)
      (define x-axis-angle (plot-dir->dc-angle (vector 1 0 0)))
      (define y-axis-angle (plot-dir->dc-angle (vector 0 1 0)))
      (define v0 (plot->dc (vector x-mid (if x-labels-y-min? y-min y-max) z-min)))
      (define dist (+ (* 1/2 (plot3d-tick-size)) (pen-gap)
                      (* (abs (cos y-axis-angle)) max-x-tick-label-width)
                      (* (abs (sin y-axis-angle)) char-height)
                      (* 1/2 char-height)))
      (define v1 (v+ v0 (v* (vector (cos y-axis-angle) (sin y-axis-angle))
                            (if x-labels-y-min? (- dist) dist))))
      (list (plot3d-x-label) v1 'top (- (if x-labels-y-min? 0 pi) x-axis-angle)))
    
    (define (get-y-label-params)
      (define x-axis-angle (plot-dir->dc-angle (vector 1 0 0)))
      (define y-axis-angle (plot-dir->dc-angle (vector 0 1 0)))
      (define v0 (plot->dc (vector (if y-labels-x-min? x-min x-max) y-mid z-min)))
      (define dist (+ (* 1/2 (plot3d-tick-size)) (pen-gap)
                      (* (abs (cos x-axis-angle)) max-y-tick-label-width)
                      (* (abs (sin x-axis-angle)) char-height)
                      (* 1/2 char-height)))
      (define v1 (v+ v0 (v* (vector (cos x-axis-angle) (sin x-axis-angle))
                            (if y-labels-x-min? (- dist) dist))))
      (list (plot3d-y-label) v1 'top (- (if y-labels-x-min? pi 0) y-axis-angle)))
    
    (define (get-z-label-params)
      (define x (if x-labels-y-min? x-min x-max))
      (define y (if y-labels-x-min? y-max y-min))
      (define v (v+ (plot->dc (vector x y z-max))
                    (vector 0 (* -1/2 char-height))))
      (list (plot3d-z-label) v 'bottom-left 0))
    
    (define (get-x-tick-label-params)
      (define y-axis-angle (plot-dir->dc-angle (vector 0 1 0)))
      (define dist (+ (pen-gap) (* 1/2 (plot3d-tick-size))))
      (define offset (v* (vector (cos y-axis-angle) (sin y-axis-angle))
                         (if x-labels-y-min? (- dist) dist)))
      (define y (if x-labels-y-min? y-min y-max))
      (define s (sin theta))
      (define anchor
        (cond [(s . < . (sin (degrees->radians -67.5)))  (if x-labels-y-min? 'top-right 'top-left)]
              [(s . < . (sin (degrees->radians -22.5)))  (if x-labels-y-min? 'top-right 'top-left)]
              [(s . < . (sin (degrees->radians 22.5)))   'top]
              [(s . < . (sin (degrees->radians 67.5)))   (if x-labels-y-min? 'top-left 'top-right)]
              [else                                      (if x-labels-y-min? 'top-left 'top-right)]))
      
      (define fx (invertible-fun-f ((plot3d-x-transform) x-min x-max)))
      (for/list ([t  (in-list (filter tick-major? x-ticks))])
        (match-define (tick x x-str major?) t)
        (list x-str (v+ (plot->dc (vector (fx x) y z-min)) offset) anchor 0)))
    
    (define (get-y-tick-label-params)
      (define x-axis-angle (plot-dir->dc-angle (vector 1 0 0)))
      (define dist (+ (pen-gap) (* 1/2 (plot3d-tick-size))))
      (define offset (v* (vector (cos x-axis-angle) (sin x-axis-angle))
                         (if y-labels-x-min? (- dist) dist)))
      (define x (if y-labels-x-min? x-min x-max))
      (define c (cos theta))
      (define anchor
        (cond [(c . > . (cos (degrees->radians 22.5)))   (if y-labels-x-min? 'top-right 'top-left)]
              [(c . > . (cos (degrees->radians 67.5)))   (if y-labels-x-min? 'top-right 'top-left)]
              [(c . > . (cos (degrees->radians 112.5)))  'top]
              [(c . > . (cos (degrees->radians 157.5)))  (if y-labels-x-min? 'top-left 'top-right)]
              [else                                      (if y-labels-x-min? 'top-left 'top-right)]))
      
      (define fy (invertible-fun-f ((plot3d-y-transform) y-min y-max)))
      (for/list ([t  (in-list (filter tick-major? y-ticks))])
        (match-define (tick y y-str major?) t)
        (list y-str (v+ (plot->dc (vector x (fy y) z-min)) offset) anchor 0)))
    
    (define (get-z-tick-label-params)
      (define dist (+ (pen-gap) (* 1/2 (plot3d-tick-size))))
      (define offset (vector (- dist) (* 2 (get-char-baseline))))
      (define x (if x-labels-y-min? x-min x-max))
      (define y (if y-labels-x-min? y-max y-min))
      
      (define fz (invertible-fun-f ((plot3d-z-transform) z-min z-max)))
      (for/list ([t  (in-list (filter tick-major? z-ticks))])
        (match-define (tick z z-str major?) t)
        (list z-str (v+ (plot->dc (vector x y (fz z))) offset) 'bottom-right 0)))
    
    (define (get-label-params)
      (append (if (plot3d-x-label) (list (get-x-label-params)) empty)
              (if (plot3d-y-label) (list (get-y-label-params)) empty)
              (if (plot3d-z-label) (list (get-z-label-params)) empty)
              (get-x-tick-label-params)
              (get-y-tick-label-params)
              (get-z-tick-label-params)))
    
    ;; We have a mutual dependence problem:
    ;; 1. We can't set the margins without knowing where the axis labels will be
    ;; 2. We can't determine the axis label angles (and thus their positions)
    ;;    without knowing the margins
    
    ;; So we:
    ;; 1. Define 'new-margins', which takes the current margins and info about
    ;;    the current labels, and returns margins large enough that the current
    ;;    axis labels would be drawn completely on the dc (although at slightly
    ;;    wrong angles)
    ;; 2. Iterate 'new-margins', recalculating the labels every iteration
    
    ;; Because 'new-margins' is monotone, the amount of axis label drawn off the
    ;; dc is zero in the limit. In practice, 5 iterations gets the margins
    ;; within 1/100 of a drawing unit in the worst cases.
    
    (define (new-margins left right top bottom axis-label-params)
      (match-define (list (vector label-xs label-ys) ...)
        (append* (map (λ (params) (send/apply this get-text-corners params)) axis-label-params)))
      
      (define label-x-min (apply min 0 label-xs))
      (define label-x-max (apply max (sub1 dc-x-size) label-xs))
      (define label-y-min (apply min 0 label-ys))
      (define label-y-max (apply max (sub1 dc-y-size) label-ys))
      
      (values (+ left (- label-x-min))
              (+ right (- label-x-max (sub1 dc-x-size)))
              (+ top (- label-y-min))
              (+ bottom (- label-y-max (sub1 dc-y-size)))))
    
    (define-values (area-x-min right area-y-min bottom)
      (for/fold ([left init-left-margin]
                 [right init-right-margin]
                 [top init-top-margin]
                 [bottom init-bottom-margin]) ([i  (in-range 5)])
        (define-values (new-left new-right new-top new-bottom)
          (new-margins left right top bottom (get-label-params)))
        (set! view->dc* (make-view->dc new-left new-right new-top new-bottom))
        ;(printf "margins: ~v ~v ~v ~v~n" new-left new-right new-top new-bottom)
        (values new-left new-right new-top new-bottom)))
    
    (define (set-major-pen) (set-pen (plot-foreground) (plot-pen-width) 'solid))
    (define (set-minor-pen) (set-pen (plot-foreground) (* 1/2 (plot-pen-width)) 'solid))
    
    (define (put-major-pen) (put-pen (plot-foreground) (plot-pen-width) 'solid))
    (define (put-minor-pen) (put-pen (plot-foreground) (* 1/2 (plot-pen-width)) 'solid))
    
    (define (put-borders)
      (put-minor-pen)
      ; x borders
      (define xs (linear-seq x-min x-max (plot3d-samples)))
      (for ([x1  (in-list xs)] [x2  (in-list (rest xs))])
        (put-line (vector x1 y-min z-min) (vector x2 y-min z-min))
        (put-line (vector x1 y-max z-min) (vector x2 y-max z-min)))
      ; y borders
      (define ys (linear-seq y-min y-max (plot3d-samples)))
      (for ([y1  (in-list ys)] [y2  (in-list (rest ys))])
        (put-line (vector x-min y1 z-min) (vector x-min y2 z-min))
        (put-line (vector x-max y1 z-min) (vector x-max y2 z-min)))
      ; z axes
      (define zs (linear-seq z-min z-max (plot3d-samples)))
      (for ([z1  (in-list zs)] [z2  (in-list (rest zs))])
        (put-line (vector x-min y-min z1) (vector x-min y-min z2))
        (put-line (vector x-max y-min z1) (vector x-max y-min z2))
        (put-line (vector x-max y-max z1) (vector x-max y-max z2))
        (put-line (vector x-min y-max z1) (vector x-min y-max z2))))
    
    (define (put-x-ticks)
      (define radius (* 1/2 (plot3d-tick-size)))
      (define angle (plot-dir->dc-angle (vector 0 1 0)))
      (define fx (invertible-fun-f ((plot3d-x-transform) x-min x-max)))
      (for ([t  (in-list x-ticks)])
        (match-define (tick x x-str major?) t)
        (if major? (put-major-pen) (put-minor-pen))
        ; x ticks on the y-min and y-max border
        (for ([y  (list y-min y-max)])
          (put-tick (vector (fx x) y z-min) radius angle))))
    
    (define (put-y-ticks)
      (define radius (* 1/2 (plot3d-tick-size)))
      (define angle (plot-dir->dc-angle (vector 1 0 0)))
      (define fy (invertible-fun-f ((plot3d-y-transform) y-min y-max)))
      (for ([t  (in-list y-ticks)])
        (match-define (tick y y-str major?) t)
        (if major? (put-major-pen) (put-minor-pen))
        ; y ticks on the x-min border
        (for ([x  (list x-min x-max)])
          (put-tick (vector x (fy y) z-min) radius angle))))
    
    (define (put-z-ticks)
      (define radius (* 1/2 (plot3d-tick-size)))
      (define angle 0)
      (define fz (invertible-fun-f ((plot3d-z-transform) z-min z-max)))
      (for ([t  (in-list z-ticks)])
        (match-define (tick z z-str major?) t)
        (if major? (put-major-pen) (put-minor-pen))
        ; z ticks on all four axes
        (for* ([x  (list x-min x-max)]
               [y  (list y-min y-max)])
          (put-tick (vector x y (fz z)) radius angle))))
    
    (define (draw-labels)
      (for ([params  (in-list (get-label-params))])
        (send/apply this draw-text params)))
    
    (define (draw-title)
      (define title-x-size (get-text-width (plot3d-title)))
      (draw-text (plot3d-title) (vector (* 1/2 dc-x-size) 0) 'top))
    
    (define/public (start-plot)
      (clear)
      (set! render-list empty)
      (put-borders)
      (put-x-ticks)
      (put-y-ticks)
      (put-z-ticks)
      (set! do-axis-transforms? #t))
    
    (define/public (end-plot)
      (set! do-axis-transforms? #f)
      (draw-render-list)
      (clip-to-none)
      (reset-drawing-params)
      (when (plot3d-title) (draw-title))
      (draw-labels))
    
    (define/public (put-angles)
      (define angle-str (format " angle = ~a " (number->string (round angle))))
      (define alt-str (format " altitude = ~a " (number->string (round altitude))))
      (define-values (angle-width angle-height baseline _angle2) (get-text-extent angle-str))
      (define-values (alt-width alt-height _alt1 _alt2) (get-text-extent alt-str))
      
      (define box-x-size (max angle-width alt-width))
      (define box-y-size (+ angle-height alt-height (* 3 baseline)))
      (define box-x-min (* 1/2 (- dc-x-size box-x-size)))
      (define box-y-min (* 1/2 (- dc-y-size box-y-size)))
      (define box-x-max (+ box-x-min box-x-size))
      (define box-y-max (+ box-y-min box-y-size))
      
      (set-alpha 1/2)
      (set-pen (plot-foreground) (* 1/2 (plot-pen-width)) 'solid)
      (set-brush (plot-background) 'solid)
      (draw-rectangle (vector box-x-min box-y-min) (vector box-x-max box-y-max))
      
      (set-alpha 1)
      (draw-text angle-str (vector box-x-min (+ box-y-min baseline))
                 'top-left #:outline? #t)
      (draw-text alt-str (vector box-x-min (+ box-y-min baseline char-height))
                 'top-left #:outline? #t))
    
    (define/public (put-legend legend-entries)
      (define gap (plot-pen-width))
      (draw-legend legend-entries (+ 0 gap) (- dc-x-size gap) (+ area-y-min gap) (- dc-y-size gap)))
    
    (define light (plot->view (vector x-mid y-mid (+ z-max (* 5 z-size)))))
    (define view-dir (vector 0 -50 0))
    
    (define (get-light-values s)
      (cond
        [(not (or (plot3d-diffuse-light?) (plot3d-specular-light?)))  (values 1.0 0.0)]
        [else
         ; common lighting values
         (define light-dir (vnormalize (v- light (rotate/rho (shape-center s)))))
         (define norm (shape-normal s))
         ; diffuse lighting: typical Lambertian surface model
         (define diff (cond [(plot3d-diffuse-light?)  (abs (vdot norm light-dir))]
                            [else  1.0]))
         ; specular highlighting: Blinn-Phong model
         (define spec (cond [(plot3d-specular-light?)
                             (define lv (vnormalize (v* (v+ light-dir view-dir) 1/2)))
                             (define cos-angle (abs (vdot norm lv)))
                             (define angle (acos (vdot norm lv)))
                             (* 32 (expt (if (cos-angle . > . 0) cos-angle 0.0) 10))]
                            [else  0.0]))
         ; ambient lighting
         (define amb (plot3d-ambient-light-value))
         ; put it all together
         (values (+ amb (* (- 1 amb) diff)) spec)]))
    
    (define (draw-shapes lst)
      (for ([s  (in-list (depth-sort lst))])
        (set-alpha (shape-alpha s))
        (match s
          ; shapes
          [(shapes alpha center ss)  (draw-shapes ss)]
          ; polygon
          [(polygon alpha center vs pen-color pen-width pen-style brush-color brush-style)
           (define-values (diff spec) (get-light-values s))
           (let ([pen-color  (map (λ (v) (+ (* v diff) spec)) pen-color)]
                 [brush-color  (map (λ (v) (+ (* v diff) spec)) brush-color)])
             (set-pen pen-color pen-width pen-style)
             (set-brush brush-color brush-style)
             (draw-polygon (map (λ (v) (view->dc v)) vs)))]
          ; line
          [(line alpha center v1 v2 pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-line (view->dc v1) (view->dc v2))]
          ; text
          [(text alpha center anchor angle str font-size font-family color)
           (set-font font-size font-family)
           (set-text-foreground color)
           (draw-text str (view->dc (rotate/rho center)) anchor angle)]
          ; glyph
          [(glyph alpha center symbol size pen-color pen-width pen-style brush-color brush-style)
           (set-pen pen-color pen-width pen-style)
           (set-brush brush-color brush-style)
           (draw-glyphs (list (view->dc (rotate/rho center))) symbol size)]
          ; tick glyph
          [(tick-glyph alpha center radius angle pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-tick (view->dc (rotate/rho center)) radius angle)]
          [_  (error 'end-plot "shape not implemented: ~e" s)])))
    
    ;; ===============================================================================================
    ;; Delayed drawing
    
    (define render-list empty)
    (define (add-shape! shape) (set! render-list (cons shape render-list)))
    (define (draw-render-list) (draw-shapes render-list))
    
    ; drawing parameters
    
    (define alpha 1)
    
    (define pen-color '(0 0 0))
    (define pen-width 1)
    (define pen-style 'solid)
    
    (define brush-color '(255 255 255))
    (define brush-style 'solid)
    
    (define background-color '(255 255 255))
    
    (define font-size 11)
    (define font-family 'roman)
    (define text-foreground '(0 0 0))
    
    ;; drawing parameter accessors
    
    ; alpha
    
    (define/public (put-alpha a) (set! alpha a))
    (define (get-alpha) alpha)
    
    ; pen
    
    (define/public (put-pen color width style)
      (set! pen-color (->color/line color))
      (set! pen-width width)
      (set! pen-style (->style/line style)))
    
    (define (get-pen-color) pen-color)
    (define (get-pen-width) pen-width)
    (define (get-pen-style) pen-style)
    
    ; brush
    
    (define/public (put-brush color style)
      (set! brush-color (->color/fill color))
      (set! brush-style (->style/fill style)))
    
    (define (get-brush-color) brush-color)
    (define (get-brush-style) brush-style)
    
    ; background color
    
    (define/public (put-background color)
      (set! background-color (->color/fill color)))
    (define (get-background) background-color)
    
    ; font
    
    (define/public (put-font-size size) (set! font-size size))
    (define/public (put-font-family family) (set! font-family family))
    
    (define/public (put-font size family)
      (put-font-size size)
      (put-font-family family))
    
    (define/public (put-text-foreground c)
      (set! text-foreground (->color/line c)))
    
    (define (get-font-size) font-size)
    (define (get-font-family) font-family)
    (define (get-text-foreground) text-foreground)
    
    ; shapes
    
    (define/public (put-line v1 v2 [c (center-coord (list v1 v2))])
      (when (and (vall-regular? v1) (vall-regular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-line v1 v2 clip-x-min clip-x-max
                                              clip-y-min clip-y-max
                                              clip-z-min clip-z-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (add-shape!
             (line (get-alpha) (plot->view/no-rho c) (plot->view v1) (plot->view v2)
                   (get-pen-color) (get-pen-width) (get-pen-style)))))))
    
    (define/public (put-lines vs)
      (for ([vs  (vregular-sublists vs)])
        (when (not (empty? vs))
          (for ([v1  (in-list vs)] [v2  (in-list (rest vs))])
            (put-line v1 v2)))))
    
    (define (add-polygon lst vs c)
      (let/ec return
        (when (not (and (andmap vall-regular? vs) (vall-regular? c)))
          (return lst))
        
        (let* ([vs  (if clipping?
                        (clip-polygon vs clip-x-min clip-x-max
                                      clip-y-min clip-y-max
                                      clip-z-min clip-z-max)
                        vs)]
               [vs  (map plot->view vs)])
          (when (empty? vs) (return lst))
          
          (cons (polygon (get-alpha) (plot->view/no-rho c) vs
                         (get-pen-color) (get-pen-width) (get-pen-style)
                         (get-brush-color) (get-brush-style))
                lst))))
    
    (define/public (put-polygon vs [c (center-coord vs)])
      (set! render-list (add-polygon render-list vs c)))
    
    (define/public (put-polygons vss [c (center-coord (flatten vss))])
      (define lst (for/fold ([lst empty]) ([vs  (in-list vss)])
                    (add-polygon lst vs (center-coord vs))))
      (when (not (empty? lst))
        (set! render-list (cons (shapes (get-alpha) (plot->view/no-rho c) lst)
                                render-list))))
    
    (define/public (put-text str v [anchor 'center] [angle 0])
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (text (get-alpha) (plot->view/no-rho v) anchor angle str
               (get-font-size) (get-font-family) (get-text-foreground)))))
    
    (define/public (put-glyphs vs symbol size)
      (for ([v  (in-list vs)])
        (when (and (vall-regular? v) (in-bounds? v))
          (add-shape!
           (glyph (get-alpha) (plot->view/no-rho v) symbol size
                  (get-pen-color) (get-pen-width) (get-pen-style)
                  (get-brush-color) (get-brush-style))))))
    
    (define/public (put-tick v radius angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (tick-glyph (get-alpha) (plot->view/no-rho v) radius angle
                     (get-pen-color) (get-pen-width) (get-pen-style)))))
    )) ; end class
