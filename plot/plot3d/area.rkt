#lang racket/base

(require racket/class racket/match racket/list racket/math
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/list.rkt"
         "../common/color.rkt"
         "transform.rkt"
         "shape.rkt"
         "clip.rkt")

(provide (all-defined-out))

(define plot3d-samples (make-parameter 40))

(define plot3d-animating? (make-parameter #f))

(define plot3d-angle (make-parameter 30))
(define plot3d-altitude (make-parameter 60))

(define plot3d-ambient-light (make-parameter 2/3))
(define plot3d-diffuse-light? (make-parameter #t))
(define plot3d-specular-light? (make-parameter #t))

(define plot3d-tick-skip (make-parameter 2))
(define plot3d-tick-size (make-parameter 10))

(define plot3d-title (make-parameter #f))
(define plot3d-x-label (make-parameter "x axis"))
(define plot3d-y-label (make-parameter "y axis"))
(define plot3d-z-label (make-parameter "z axis"))

(define plot3d-foreground (make-parameter "black"))
(define plot3d-background (make-parameter "white"))

(define plot3d-font-size (make-parameter 11))
(define plot3d-font-family (make-parameter 'roman))

(define plot3d-pen-width (make-parameter 1))
(define (plot3d-pen-gap) (* 2 (plot3d-pen-width)))

;; =============================================================================
;; Plot area

(define 3d-plot-area%
  (class plot-area%
    (init-field x-ticks y-ticks z-ticks
                x-min x-max y-min y-max z-min z-max)
    (init the-dc)
    (inherit set-font set-text-foreground
             get-font-size get-font-family get-text-foreground
             get-text-width get-char-height get-size get-text-extent
             get-text-corners
             set-pen get-pen-color get-pen-width get-pen-style
             set-brush get-brush-color get-brush-style 
             set-alpha get-alpha 
             set-background
             clear draw-text/raw draw-polygon draw-line draw-text draw-point
             draw-circle-glyph draw-polygon-glyph draw-flare-glyph
             draw-arrow-glyph)
    
    (super-make-object the-dc)
    
    (define (reset-drawing-params)
      (set-font (plot3d-font-size) (plot3d-font-family))
      (set-text-foreground (plot3d-foreground))
      (set-pen (plot3d-foreground) (plot3d-pen-width) 'solid)
      (set-brush (plot3d-background) 'solid)
      (set-background (plot3d-background))
      (set-alpha 1))
    
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
      (define cx-min (if rx-min (max x-min rx-min) x-min))
      (define cx-max (if rx-max (min x-max rx-max) x-max))
      (define cy-min (if ry-min (max y-min ry-min) y-min))
      (define cy-max (if ry-max (min y-max ry-max) y-max))
      (define cz-min (if rz-min (max z-min rz-min) z-min))
      (define cz-max (if rz-max (min z-max rz-max) z-max))
      (let ([cx-min  (min cx-min cx-max)]
            [cx-max  (max cx-min cx-max)]
            [cy-min  (min cy-min cy-max)]
            [cy-max  (max cy-min cy-max)]
            [cz-min  (min cz-min cz-max)]
            [cz-max  (max cz-min cz-max)])
        (set! clip-x-min cx-min)
        (set! clip-x-max cx-max)
        (set! clip-y-min cy-min)
        (set! clip-y-max cy-max)
        (set! clip-z-min cz-min)
        (set! clip-z-max cz-max)))
    
    (define (clip-to-none)
      (set! clipping? #f))
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-z-min) z-min)
    (define/public (get-z-max) z-max)
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define z-size (- z-max z-min))
    
    (define x-mid (* 1/2 (+ x-min x-max)))
    (define y-mid (* 1/2 (+ y-min y-max)))
    (define z-mid (* 1/2 (+ z-min z-max)))
    
    (define angle (plot3d-angle))
    (define altitude (plot3d-altitude))
    ; FP HACK: adding an epsilon to the angle ensures, when it is 90, 180
    ; or 270, that the x or y labels are drawn on the left side and that the
    ; z labels are drawn on the left
    (define theta (+ (degrees->radians angle) 0.00001))
    (define rho (degrees->radians altitude))
    
    (define transform/no-rho-m3
      (m3* (m3make-rotate-z theta)
           (m3make-scale (/ x-size) (/ y-size) (/ z-size))))
    (define transform-m3 (m3* (m3make-rotate-x rho) transform/no-rho-m3))
    (define invtransform-m3 (m3transpose transform-m3))
    
    (define (center v)
      (match-define (vector x y z) v)
      (vector (- x x-mid) (- y y-mid) (- z z-mid)))
    
    (define (plot->view v) (m3apply transform-m3 (center v)))
    (define (plot->view/no-rho v) (m3apply transform/no-rho-m3 (center v)))
    (define (rotate/rho v) (m3apply (m3make-rotate-x rho) v))
    
    (define plot-right-dir (m3apply invtransform-m3 (vector 1 0 0)))
    (define plot-up-dir (m3apply invtransform-m3 (vector 0 0 1)))
    
    (define-values (dc-x-size dc-y-size) (get-size))
    
    (define view->dc* #f)
    (define (plot->dc v) (view->dc* (plot->view v)))

    (define (plot-dir->dc-angle v)
      (match-define (vector dx dy) (v- (plot->dc v) (plot->dc (vector 0 0 0))))
      (atan2 (- dy) dx))

    (define ((make-dc-size->plot-size v) size)
      (/ size (vmag (v- (plot->dc v) (plot->dc (vector 0 0 0))))))
    
    (define dc-size->plot-x-size (make-dc-size->plot-size (vector 1 0 0)))
    (define dc-size->plot-y-size (make-dc-size->plot-size (vector 0 1 0)))
    (define dc-size->plot-h-size (make-dc-size->plot-size plot-right-dir))
    (define dc-size->plot-v-size (make-dc-size->plot-size plot-up-dir))
    
    (define/override (view->dc v) (view->dc* v))
    
    ;; Initial plot area margins leave enough room for the ticks
    
    (define init-left-margin (* 1/2 (plot3d-tick-size)))
    (define init-right-margin (* 1/2 (plot3d-tick-size)))
    (define init-top-margin (if (plot3d-title) (* 3/2 (get-char-height)) 0))
    (define init-bottom-margin (* 1/2 (plot3d-tick-size)))
    
    (define (make-view->dc area-x-min right area-y-min bottom)
      (define corners
        (list (vector x-min y-min z-min) (vector x-min y-min z-max)
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
        (let ([x  (* x area-to-view-x)]
              [z  (* z area-to-view-z)])
          (vector (+ area-x-mid x) (- area-y-mid z)))))
    
    (set! view->dc* (make-view->dc init-left-margin init-right-margin
                                   init-top-margin init-bottom-margin))
    
    ;; Label drawing constants
    
    (define x-labels-y-min? ((cos theta) . >= . 0))
    (define y-labels-x-min? ((sin theta) . >= . 0))
    
    (define max-x-tick-label-width
      (apply max (map (λ (t) (get-text-width (tick-label t)))
                      x-ticks)))
    
    (define max-y-tick-label-width
      (apply max (map (λ (t) (get-text-width (tick-label t)))
                      y-ticks)))
    
    ;; Label drawing parameters
    
    (define (get-x-label-params)
      (define x-axis-angle (plot-dir->dc-angle (vector 1 0 0)))
      (define y-axis-angle (plot-dir->dc-angle (vector 0 1 0)))
      (define offset (dc-size->plot-y-size
                      (+ (* 1/2 (plot3d-tick-size)) (plot3d-pen-gap)
                         (* (abs (cos y-axis-angle)) max-x-tick-label-width)
                         (* (abs (sin y-axis-angle)) char-height)
                         (* 1/2 char-height))))
      (define y (if x-labels-y-min? (- y-min offset) (+ y-max offset)))
      (list (plot3d-x-label) (plot->view (vector x-mid y z-min)) 'top
            (+ x-axis-angle (if x-labels-y-min? 0 pi))))
    
    (define (get-y-label-params)
      (define x-axis-angle (plot-dir->dc-angle (vector 1 0 0)))
      (define y-axis-angle (plot-dir->dc-angle (vector 0 1 0)))
      (define offset (dc-size->plot-x-size
                      (+ (* 1/2 (plot3d-tick-size)) (plot3d-pen-gap)
                         (* (abs (cos x-axis-angle)) max-y-tick-label-width)
                         (* (abs (sin x-axis-angle)) char-height)
                         (* 1/2 char-height))))
      (define x (if y-labels-x-min? (- x-min offset) (+ x-max offset)))
      (list (plot3d-y-label) (plot->view (vector x y-mid z-min)) 'top
            (+ y-axis-angle (if y-labels-x-min? pi 0))))
    
    (define (get-z-label-params)
      (define offset (dc-size->plot-v-size (* 1/2 char-height)))
      (match-define (vector dx dy dz) (v* plot-up-dir offset))
      (define x (if x-labels-y-min? x-min x-max))
      (define y (if y-labels-x-min? y-max y-min))
      (list (plot3d-z-label)
            (plot->view (vector (+ x dx) (+ y dy) (+ z-max dz)))
            'bl 0))
    
    (define (get-x-tick-label-params)
      (define half (dc-size->plot-y-size (* 1/2 (plot3d-tick-size))))
      (define offset (+ half (dc-size->plot-y-size (plot3d-pen-gap))))
      (for/fold ([params  empty]) ([t  (in-list x-ticks)])
        (match-define (tick x x-str major?) t)
        (cond [major?
               (define y (if x-labels-y-min? (- y-min offset) (+ y-max offset)))
               (define anchor
                 (cond [((sin theta) . < . (sin (degrees->radians -67.5)))
                        (if x-labels-y-min? 'top-right 'top-left)]
                       [((sin theta) . < . (sin (degrees->radians -22.5)))
                        (if x-labels-y-min? 'top-right 'top-left)]
                       [((sin theta) . < . (sin (degrees->radians 22.5)))
                        'top]
                       [((sin theta) . < . (sin (degrees->radians 67.5)))
                        (if x-labels-y-min? 'top-left 'top-right)]
                       [else
                        (if x-labels-y-min? 'top-left 'top-right)]))
               (cons (list x-str (plot->view (vector x y z-min))
                           anchor 0)
                     params)]
              [else  params])))
    
    (define (get-y-tick-label-params)
      (define half (dc-size->plot-x-size (* 1/2 (plot3d-tick-size))))
      (define offset (+ half (dc-size->plot-x-size (plot3d-pen-gap))))
      (for/fold ([params  empty]) ([t  (in-list y-ticks)])
        (match-define (tick y y-str major?) t)
        (cond [major?
               (define x (if y-labels-x-min? (- x-min offset) (+ x-max offset)))
               (define anchor
                 (cond [((cos theta) . > . (cos (degrees->radians 22.5)))
                        (if y-labels-x-min? 'top-right 'top-left)]
                       [((cos theta) . > . (cos (degrees->radians 67.5)))
                        (if y-labels-x-min? 'top-right 'top-left)]
                       [((cos theta) . > . (cos (degrees->radians 112.5)))
                        'top]
                       [((cos theta) . > . (cos (degrees->radians 157.5)))
                        (if y-labels-x-min? 'top-left 'top-right)]
                       [else
                        (if y-labels-x-min? 'top-left 'top-right)]))
               (cons (list y-str (plot->view (vector x y z-min))
                           anchor 0)
                     params)]
              [else  params])))
    
    (define (get-z-tick-label-params)
      (define half (dc-size->plot-h-size (* 1/2 (plot3d-tick-size))))
      (define offset (+ half (dc-size->plot-h-size (plot3d-pen-gap))))
      (match-define (vector ldx ldy _ldz) (v* plot-right-dir offset))
      (define lx (if x-labels-y-min? x-min x-max))
      (define ly (if y-labels-x-min? y-max y-min))
      (for/fold ([params  empty]) ([t  (in-list z-ticks)])
        (match-define (tick z z-str major?) t)
        (cond [major?
               (cons (list z-str (plot->view (vector (- lx ldx) (- ly ldy) z))
                           'right 0)
                     params)]
              [else  params])))
    
    (define (get-label-params)
      (append (if (plot3d-x-label) (list (get-x-label-params)) empty)
              (if (plot3d-y-label) (list (get-y-label-params)) empty)
              (if (plot3d-z-label) (list (get-z-label-params)) empty)
              (get-x-tick-label-params)
              (get-y-tick-label-params)
              (get-z-tick-label-params)))
    
    ;; We have a mutual dependency problem:
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
        (append* (map (λ (params) (send/apply this get-text-corners params))
                      axis-label-params)))
      
      (define label-x-min (apply min 0 label-xs))
      (define label-x-max (apply max (sub1 dc-x-size) label-xs))
      (define label-y-min (apply min 0 label-ys))
      (define label-y-max (apply max (sub1 dc-y-size) label-ys))
      
      (values (+ left (- label-x-min))
              (+ right (- label-x-max (sub1 dc-x-size)))
              (+ top (- label-y-min))
              (+ bottom (- label-y-max (sub1 dc-y-size)))))
    
    (for/fold ([left init-left-margin]
               [right init-right-margin]
               [top init-top-margin]
               [bottom init-bottom-margin])
              ([i  (in-range 5)])
      (define-values (new-left new-right new-top new-bottom)
        (new-margins left right top bottom (get-label-params)))
      (set! view->dc* (make-view->dc new-left new-right new-top new-bottom))
      ;(printf "margins: ~v ~v ~v ~v~n" new-left new-right new-top new-bottom)
      (values new-left new-right new-top new-bottom))
    
    (define (set-major-pen)
      (set-pen (plot3d-foreground) (plot3d-pen-width) 'solid))
    
    (define (set-minor-pen)
      (set-pen (plot3d-foreground) (* 1/2 (plot3d-pen-width)) 'solid))
    
    (define (add-borders)
      (set-minor-pen)
      ; x borders
      (define xs (real-seq x-min x-max (plot3d-samples)))
      (for ([x1  (in-list xs)]
            [x2  (in-list (rest xs))])
        (add-line (vector x1 y-min z-min) (vector x2 y-min z-min))
        (add-line (vector x1 y-max z-min) (vector x2 y-max z-min)))
      ; y borders
      (define ys (real-seq y-min y-max (plot3d-samples)))
      (for ([y1  (in-list ys)]
            [y2  (in-list (rest ys))])
        (add-line (vector x-min y1 z-min) (vector x-min y2 z-min))
        (add-line (vector x-max y1 z-min) (vector x-max y2 z-min)))
      ; z axes
      (define zs (real-seq z-min z-max (plot3d-samples)))
      (for ([z1  (in-list zs)]
            [z2  (in-list (rest zs))])
        (add-line (vector x-min y-min z1) (vector x-min y-min z2))
        (add-line (vector x-max y-min z1) (vector x-max y-min z2))
        (add-line (vector x-max y-max z1) (vector x-max y-max z2))
        (add-line (vector x-min y-max z1) (vector x-min y-max z2))))
    
    (define (add-x-ticks)
      (define half (dc-size->plot-y-size (* 1/2 (plot3d-tick-size))))
      (define offset (+ half (dc-size->plot-y-size (plot3d-pen-gap))))
      (for ([t  (in-list x-ticks)])
        (match-define (tick x x-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        ; x ticks on the y-min and y-max border
        (for ([y  (list y-min y-max)])
          (add-line (vector x (+ y half) z-min)
                    (vector x (- y half) z-min)))))
    
    (define (add-y-ticks)
      (define half (dc-size->plot-x-size (* 1/2 (plot3d-tick-size))))
      (define offset (+ half (dc-size->plot-x-size (plot3d-pen-gap))))
      (for ([t  (in-list y-ticks)])
        (match-define (tick y y-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        ; y ticks on the x-min border
        (for ([x  (list x-min x-max)])
          (add-line (vector (+ x half) y z-min)
                    (vector (- x half) y z-min)))))
    
    (define (add-z-ticks)
      (define half (dc-size->plot-h-size (* 1/2 (plot3d-tick-size))))
      (define offset (+ half (dc-size->plot-h-size (plot3d-pen-gap))))
      (match-define (vector dx dy _dz) (v* plot-right-dir half))
      (for ([t  (in-list z-ticks)])
        (match-define (tick z z-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        ; z ticks on all four axes
        (for* ([x  (list x-min x-max)] [y  (list y-min y-max)])
          (add-line (vector (- x dx) (- y dy) z)
                    (vector (+ x dx) (+ y dy) z)))))
    
    (define (draw-labels)
      (for ([params  (in-list (get-label-params))])
        (send/apply this draw-text params)))
    
    (define (draw-title)
      (define-values (title-x-size _1 _2 _3)
        (get-text-extent (plot3d-title)))
      (draw-text/raw (plot3d-title) (* 1/2 dc-x-size) 0 'top))
    
    (define (draw-angles)
      (define x 2)
      (define y 2)
      (draw-text/raw (format "angle = ~a" (real->tick-label angle))
                     2 2 'top-left #:outline? #t)
      (draw-text/raw (format "altitude = ~a" (real->tick-label altitude))
                     2 (+ 4 char-height) 'top-left #:outline? #t))
    
    (define render-list empty)
    
    (define (add-shape! shape)
      (set! render-list (cons shape render-list)))
    
    (define/public (start-plot)
      (clear)
      (set! render-list empty)
      (add-borders)
      (add-x-ticks)
      (add-y-ticks)
      (add-z-ticks))
    
    (define/public (end-plot)
      (draw-shapes)
      (reset-drawing-params)
      (clip-to-none)
      (when (plot3d-title) (draw-title))
      (draw-labels)
      (when (plot3d-animating?) (draw-angles)))
    
    (define light
      (plot->view (vector x-mid y-mid (+ z-max (* 5 z-size)))))
    
    (define (get-light-values s)
      (let/ec return
        (when (and (not (plot3d-diffuse-light?))
                   (not (plot3d-specular-light?)))
          (return 1.0 0.0))
        ; common lighting values
        (define light-dir (let ([center  (rotate/rho (shape-center s))])
                            (vnormalize (v- light center))))
        (define norm (shape-normal s))
        ; diffuse lighting: typical Lambertian surface model
        (define diff
          (cond [(plot3d-diffuse-light?)  (vdot norm light-dir)]
                [else  1.0]))
        ; ambient lighting
        (define amb (plot3d-ambient-light))
        ; specular highlighting: Blinn-Phong model
        (define spec
          (cond [(plot3d-specular-light?)
                 (define view-dir (vector 0 -1 0))
                 (define lv (vnormalize (v* (v+ light-dir view-dir) 1/2)))
                 (define cos-angle (vdot norm lv))
                 (define angle (acos (vdot norm lv)))
                 (* 64 (expt (if (cos-angle . > . 0) cos-angle 0.0) 5))]
                [else  0.0]))
        (values (+ amb (* (- 1 amb) diff)) spec)))
    
    (define (draw-shapes)
      (for ([s  (in-list (depth-sort render-list))])
        (set-alpha (shape-alpha s))
        (match s
          ; polygon
          [(polygon alpha center vs
                    pen-color pen-width pen-style
                    brush-color brush-style)
           (define-values (diff spec) (get-light-values s))
           (let ([pen-color  (map (λ (v) (+ (* v diff) spec)) pen-color)]
                 [brush-color  (map (λ (v) (+ (* v diff) spec)) brush-color)])
             (set-pen pen-color pen-width pen-style)
             (set-brush brush-color brush-style)
             (draw-polygon vs))]
          ; line
          [(line alpha center v1 v2 pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-line v1 v2)]
          ; text
          [(text alpha center anchor angle str font-size font-family color)
           (set-font font-size font-family)
           (set-text-foreground color)
           (draw-text str (rotate/rho center) anchor angle)]
          ; point
          [(point alpha center pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-point (rotate/rho center))]
          ; circle
          [(circle-glyph alpha center r
                         pen-color pen-width pen-style
                         brush-color brush-style)
           (set-pen pen-color pen-width pen-style)
           (set-brush brush-color brush-style)
           (draw-circle-glyph (rotate/rho center) r)]
          ; regular polygon
          [(polygon-glyph alpha center r sides start-angle
                          pen-color pen-width pen-style
                          brush-color brush-style)
           (set-pen pen-color pen-width pen-style)
           (set-brush brush-color brush-style)
           (draw-polygon-glyph (rotate/rho center) r sides start-angle)]
          ; flare (plus, asterisk, etc.)
          [(flare-glyph alpha center r sticks start-angle
                        pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-flare-glyph (rotate/rho center) r sticks start-angle)]
          ; arrow
          [(arrow-glyph alpha center r angle pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-arrow-glyph (rotate/rho center) r angle)]
          [_  (error 'end-plot "shape not implemented: ~e" s)])))
    
    (define/public (add-line/center v1 v2 c)
      (when (and (vall-regular? v1) (vall-regular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-line v1 v2 clip-x-min clip-x-max
                                              clip-y-min clip-y-max
                                              clip-z-min clip-z-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (add-shape!
             (line (get-alpha) (plot->view/no-rho c)
                   (plot->view v1) (plot->view v2)
                   (get-pen-color) (get-pen-width) (get-pen-style)))))))
    
    (define/public (add-line v1 v2)
      (add-line/center v1 v2 (center-coord (list v1 v2))))
    
    (define/public (add-lines vs)
      (for ([vs  (vregular-sublists vs)])
        (when (not (empty? vs))
          (for ([v1  (in-list vs)]
                [v2  (in-list (rest vs))])
            (add-line v1 v2)))))
    
    (define/public (add-polygon/center vs c)
      (when (and (andmap vall-regular? vs) (vall-regular? c))
        (let* ([vs  (if clipping?
                        (clip-polygon vs clip-x-min clip-x-max
                                      clip-y-min clip-y-max
                                      clip-z-min clip-z-max)
                        vs)]
               [vs  (map plot->view vs)])
          (when (not (empty? vs))
            (add-shape!
             (polygon (get-alpha) (plot->view/no-rho c)
                      vs (get-pen-color) (get-pen-width) (get-pen-style)
                      (get-brush-color) (get-brush-style)))))))
    
    (define/public (add-polygon vs)
      (add-polygon/center vs (center-coord vs)))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max
                            clip-y-min clip-y-max
                            clip-z-min clip-z-max)))
    
    (define/public (add-text str v [anchor 'center] [angle 0])
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (text (get-alpha) (plot->view/no-rho v)
               anchor angle str (get-font-size) (get-font-family)
               (get-text-foreground)))))
    
    (define/public (add-point v)
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (point (get-alpha) (plot->view/no-rho v)
                (get-pen-color) (get-pen-width) (get-pen-style)))))
    
    (define/public (add-circle-glyph v r)
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (circle-glyph (get-alpha) (plot->view/no-rho v)
                       r (get-pen-color) (get-pen-width) (get-pen-style)
                       (get-brush-color) (get-brush-style)))))
    
    (define/public (add-polygon-glyph v r sides start-angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (polygon-glyph (get-alpha) (plot->view/no-rho v)
                        r sides start-angle
                        (get-pen-color) (get-pen-width) (get-pen-style)
                        (get-brush-color) (get-brush-style)))))
    
    (define/public (add-flare-glyph v r sticks start-angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (flare-glyph (get-alpha) (plot->view/no-rho v)
                      r sticks start-angle
                      (get-pen-color) (get-pen-width) (get-pen-style)))))
    
    (define/public (add-arrow-glyph v r angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (add-shape!
         (arrow-glyph (get-alpha) (plot->view/no-rho v)
                      r angle
                      (get-pen-color) (get-pen-width) (get-pen-style)))))
    
    (define/public (add-text-glyph v str)
      (add-text str v))
    ))
