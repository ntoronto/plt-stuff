#lang racket/base

(require racket/gui racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/area.rkt"
         "../common/ticks.rkt"
         "shape.rkt"
         "clip.rkt")

(provide (all-defined-out))

(define plot3d-samples (make-parameter 40))

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

(define plot3d-fg-color (make-parameter "black"))
(define plot3d-bg-color (make-parameter "white"))

(define plot3d-font-size (make-parameter 8))
(define plot3d-font-family (make-parameter 'roman))

(define plot3d-pen-width (make-parameter 1))
(define (plot3d-pen-gap) (* 2 (plot3d-pen-width)))

;; =============================================================================
;; Plot area

(define 3d-plot-area%
  (class plot-area%
    (init-field x-min x-max y-min y-max z-min z-max)
    (init the-dc)
    
    (super-make-object the-dc)
    
    (send this set-font (plot3d-font-size) (plot3d-font-family))
    (send this set-text-foreground (plot3d-fg-color))
    (send this set-pen (plot3d-fg-color) (plot3d-pen-width) 'solid)
    (send this set-brush (plot3d-bg-color) 'solid)
    (send this set-background (plot3d-bg-color))
    (send this set-alpha 1.0)
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define z-size (- z-max z-min))
    
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    (define clip-z-min z-min)
    (define clip-z-max z-max)
    
    (define/public (clip-to-bounds rx-min rx-max ry-min ry-max rz-min rz-max)
      (set! clipping? #t)
      (when rx-min (set! clip-x-min (max x-min rx-min)))
      (when rx-max (set! clip-x-max (min x-max rx-max)))
      (when ry-min (set! clip-y-min (max y-min ry-min)))
      (when ry-max (set! clip-y-max (min y-max ry-max)))
      (when rz-min (set! clip-z-min (max z-min rz-min)))
      (when rz-max (set! clip-z-max (min z-max rz-max))))
    
    (define-values (dc-x-size dc-y-size) (send this get-size))
    (define char-width (send this get-char-width))
    (define char-height (send this get-char-height))
    
    (define x-margin
      (+ (* 10 char-width)
         (plot3d-pen-gap)
         (* 1/2 (plot3d-tick-size))))
    
    (define area-x-size
      (- dc-x-size (* 2 x-margin)))
    
    (define y-margin
      (+ (* 1/2 (plot3d-tick-size))
         (plot3d-pen-gap)
         char-height
         (* 2 char-height)))
    
    (define area-y-size
      (- dc-y-size
         y-margin
         (* 3/2 char-height)  ; z axis label
         (if (plot3d-title)
             (* 3/2 char-height)
             0)))
    
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-z-min) z-min)
    (define/public (get-z-max) z-max)
    
    (define x-mid (* 1/2 (+ x-min x-max)))
    (define y-mid (* 1/2 (+ y-min y-max)))
    (define z-mid (* 1/2 (+ z-min z-max)))
    
    (define theta (degrees->radians (plot3d-angle)))
    (define rho (degrees->radians (plot3d-altitude)))
    
    (define cos-theta (cos theta))
    (define sin-theta (sin theta))
    (define cos-rho (cos rho))
    (define sin-rho (sin rho))
    
    (define (plot->view v)
      (match-define (vector x y z) v)
      ; translate to origin, scale to cube [-1,1] x [-1,1] x [-1,1]
      (let ([x  (/ (- x x-mid) x-size)]
            [y  (/ (- y y-mid) y-size)]
            [z  (/ (- z z-mid) z-size)])
        ; rotate theta
        (let ([x  (- (* cos-theta x) (* sin-theta y))]
              [y  (+ (* sin-theta x) (* cos-theta y))])
          ; rotate rho
          (let ([y  (- (* cos-rho y) (* sin-rho z))]
                [z  (+ (* sin-rho y) (* cos-rho z))])
            (vector x y z)))))
    
    (define corners
      (list (vector x-min y-min z-min) (vector x-min y-min z-max)
            (vector x-min y-max z-min) (vector x-min y-max z-max)
            (vector x-max y-min z-min) (vector x-max y-min z-max)
            (vector x-max y-max z-min) (vector x-max y-max z-max)))
    
    (define view-corners (map plot->view corners))
    
    (define-values
      (view-x-min view-x-max view-y-min view-y-max view-z-min view-z-max)
      (let ([xs  (map (λ (v) (vector-ref v 0)) view-corners)]
            [ys  (map (λ (v) (vector-ref v 1)) view-corners)]
            [zs  (map (λ (v) (vector-ref v 2)) view-corners)])
        (values (apply min xs) (apply max xs)
                (apply min ys) (apply max ys)
                (apply min zs) (apply max zs))))
    
    (define area-to-view-x (/ area-x-size (- view-x-max view-x-min)))
    (define area-to-view-z (/ area-y-size (- view-z-max view-z-min)))
    
    (define area-x-size/2 (* 1/2 area-x-size))
    (define area-y-size/2 (* 1/2 area-y-size))
    
    (define/override (view->dc v)
      (match-define (vector x y z) v)
      (let ([x  (* x area-to-view-x)]
            [z  (* z area-to-view-z)])
        (vector (+ x-margin area-x-size/2 x)
                (- dc-y-size (+ y-margin area-y-size/2 z)))))
    
    (define/public (plot->dc v)
      (view->dc (plot->view v)))
    
    (define (plot-dir->dc-mag v)
      (vmag (v- (plot->dc v) (plot->dc (vector 0 0 0)))))
    
    (define (plot-dir->dc-angle v)
      (match-define (vector dx dy) (v- (plot->dc v) (plot->dc (vector 0 0 0))))
      (atan2 (- dy) dx))
    
    (define dc-unit/x-axis (/ 1 (plot-dir->dc-mag (vector 1 0 0))))
    (define (dc->plot/x-size x)
      (* x dc-unit/x-axis))
    
    (define dc-unit/y-axis (/ 1 (plot-dir->dc-mag (vector 0 1 0))))
    (define (dc->plot/y-size y)
      (* y dc-unit/y-axis))
    
    (define dc-unit/z-axis (/ 1 (plot-dir->dc-mag (vector 0 0 1))))
    (define (dc->plot/z-size y)
      (* y dc-unit/z-axis))
    
    (define dc-unit/horiz (/ 1 (plot-dir->dc-mag
                                (vnormalize (vector (* x-size cos-theta)
                                                    (* y-size (- sin-theta))
                                                    0)))))
    (define (dc->plot/horiz-size h)
      (* h dc-unit/horiz))
    
    (define (set-major-pen)
      (send this set-pen (plot3d-fg-color) (plot3d-pen-width) 'solid))
    
    (define (set-minor-pen)
      (send this set-pen (plot3d-fg-color) (* 1/2 (plot3d-pen-width)) 'solid))
    
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
    
    (define (add-x-ticks ticks)
      (define x-tick-half (dc->plot/y-size (* 1/2 (plot3d-tick-size))))
      (define x-tick-label-offset
        (dc->plot/y-size (+ (plot3d-pen-gap) (* 1/2 (plot3d-tick-size)))))
      (for ([t  (in-list ticks)])
        (match-define (tick x x-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (add-line (vector x (+ y-min x-tick-half) z-min)
                  (vector x (- y-min x-tick-half) z-min))
        (add-line (vector x (- y-max x-tick-half) z-min)
                  (vector x (+ y-max x-tick-half) z-min))
        (when major?
          (define y
            (cond [(positive? (cos theta))  (- y-min x-tick-label-offset)]
                  [else                     (+ y-max x-tick-label-offset)]))
          (define anchor
            (cond
              [((sin theta) . < . (sin (degrees->radians -67.5)))
               (if (positive? (cos theta)) 'right 'left)]
              [((sin theta) . < . (sin (degrees->radians -22.5)))
               (if (positive? (cos theta)) 'top-right 'top-left)]
              [((sin theta) . < . (sin (degrees->radians 22.5)))   'top]
              [((sin theta) . < . (sin (degrees->radians 67.5)))
               (if (positive? (cos theta)) 'top-left 'top-right)]
              [else
               (if (positive? (cos theta)) 'left 'right)]))
          (add-text x-str (vector x y z-min) anchor))))
    
    (define (add-x-label)
      (define x (* 1/2 (+ x-min x-max)))
      (define offset
        (dc->plot/y-size (+ (* 1/2 (plot3d-tick-size))
                            (plot3d-pen-gap)
                            (* 2 char-height))))
      (define-values (y angle-offset)
        (cond [(positive? (cos theta))  (values (- y-min offset) 0)]
              [else                     (values (+ y-max offset) pi)]))
      (define angle (+ angle-offset (plot-dir->dc-angle (vector 1 0 0))))
      (add-text (plot3d-x-label) (vector x y z-min) 'top angle))
    
    (define (add-y-ticks ticks)
      (define y-tick-half (dc->plot/x-size (* 1/2 (plot3d-tick-size))))
      (define y-tick-label-offset
        (dc->plot/x-size (+ (plot3d-pen-gap) (* 1/2 (plot3d-tick-size)))))
      (for ([t  (in-list ticks)])
        (match-define (tick y y-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (add-line (vector (+ x-min y-tick-half) y z-min)
                  (vector (- x-min y-tick-half) y z-min))
        (add-line (vector (- x-max y-tick-half) y z-min)
                  (vector (+ x-max y-tick-half) y z-min))
        (when major?
          (define x
            (cond [(negative? (sin theta))  (+ x-max y-tick-label-offset)]
                  [else                     (- x-min y-tick-label-offset)]))
          (define anchor
            (cond
              [((cos theta) . > . (cos (degrees->radians 22.5)))
               (if (negative? (sin theta)) 'left 'right)]
              [((cos theta) . > . (cos (degrees->radians 67.5)))
               (if (negative? (sin theta)) 'top-left 'top-right)]
              [((cos theta) . > . (cos (degrees->radians 112.5)))  'top]
              [((cos theta) . > . (cos (degrees->radians 157.5)))
               (if (negative? (sin theta)) 'top-right 'top-left)]
              [else
               (if (negative? (sin theta)) 'right 'left)]))
          (add-text y-str (vector x y z-min) anchor))))
    
    (define (add-y-label)
      (define y (* 1/2 (+ y-min y-max)))
      (define offset
        (dc->plot/x-size (+ (* 1/2 (plot3d-tick-size))
                            (plot3d-pen-gap)
                            (* 2 char-height))))
      (define-values (x angle-offset)
        (cond [(negative? (sin theta))  (values (+ x-max offset) 0)]
              [else                     (values (- x-min offset) pi)]))
      (define angle (+ angle-offset (plot-dir->dc-angle (vector 0 1 0))))
      (add-text (plot3d-y-label) (vector x y z-min) 'top angle))
    
    (define (add-z-ticks ticks)
      (define half (dc->plot/horiz-size (* 1/2 (plot3d-tick-size))))
      (define offset (dc->plot/horiz-size (+ (plot3d-pen-gap)
                                             (* 1/2 (plot3d-tick-size)))))
      (define horiz (vnormalize (vector (* x-size (cos (- theta)))
                                        (* y-size (sin (- theta))))))
      (match-define (vector dx dy) (v* horiz half))
      (match-define (vector ldx ldy) (v* horiz offset))
      (define x (if (positive? (cos theta)) x-min x-max))
      (define y (if (negative? (sin theta)) y-min y-max))
      (for ([t  (in-list ticks)])
        (match-define (tick z z-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (add-line (vector (- x dx) (- y dy) z)
                  (vector (+ x dx) (+ y dy) z))
        (when major?
          (add-text z-str (vector (- x ldx) (- y ldy) z) 'right))))
    
    (define (add-z-label)
      (define offset (dc->plot/z-size (* 1/2 char-height)))
      (define x (cond [(positive? (cos theta))  x-min]
                      [else                     x-max]))
      (define y (cond [(negative? (sin theta))  y-min]
                      [else                     y-max]))
      (add-text (plot3d-z-label) (vector x y (+ z-max offset)) 'bottom))
    
    (define render-list empty)
    
    (define (add-shape! shape)
      (set! render-list (cons shape render-list)))
    
    (define (add-title)
      (when (plot3d-title)
        (define-values (title-x-size _1 _2 _3)
          (send this get-text-extent (plot3d-title)))
        (send this draw-text/raw (plot3d-title) (* 1/2 dc-x-size) 0 'top)))
    
    (define/public (start-plot x-ticks y-ticks z-ticks)
      (send this clear)
      (add-title)
      
      (set! render-list empty)
      (add-borders)
      (add-x-ticks (x-ticks x-min x-max))
      (add-y-ticks (y-ticks y-min y-max))
      (add-z-ticks (z-ticks z-min z-max))
      (add-x-label)
      (add-y-label)
      (add-z-label))
    
    (define light (plot->view (vector (* 1/2 (+ x-min x-max))
                                      (* 1/2 (+ y-min y-max))
                                      (+ z-max (* 5 (- z-max z-min))))))
    
    (define (get-light-values s)
      (let/ec return
        (when (and (not (plot3d-diffuse-light?))
                   (not (plot3d-specular-light?)))
          (return 1.0 0.0))
        ; common lighting values
        (define light-dir (let ([center  (shape-center s)])
                            (vnormalize (v- light center))))
        (define norm (shape-normal s))
        (when (zero? (vmag^2 norm))
          (return 0.0 0.0))
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
    
    (define/public (end-plot)
      (for ([s  (in-list (depth-sort render-list))])
        (send this set-alpha (shape-alpha s))
        (match s
          ; polygon
          [(polygon alpha center vs
                    pen-color pen-width pen-style
                    brush-color brush-style)
           (define-values (diff spec) (get-light-values s))
           (let ([brush-color  (map (λ (v) (+ (* v diff) spec)) brush-color)])
             (send this set-pen pen-color pen-width pen-style)
             (send this set-brush brush-color brush-style)
             (send this draw-polygon vs))]
          ; line
          [(line alpha center v1 v2 pen-color pen-width pen-style)
           (send this set-pen pen-color pen-width pen-style)
           (send this draw-line v1 v2)]
          ; text
          [(text alpha v anchor angle str font-size font-family color)
           (send this set-font font-size font-family)
           (send this set-text-foreground color)
           (send this draw-text str v anchor angle)]
          ; point
          [(point alpha v pen-color pen-width pen-style)
           (send this set-pen pen-color pen-width pen-style)
           (send this draw-point v)]
          ; circle
          [(circle-glyph alpha v r
                         pen-color pen-width pen-style
                         brush-color brush-style)
           (send this set-pen pen-color pen-width pen-style)
           (send this set-brush brush-color brush-style)
           (send this draw-circle-glyph v r)]
          ; regular polygon
          [(polygon-glyph alpha v r sides start-angle
                          pen-color pen-width pen-style
                          brush-color brush-style)
           (send this set-pen pen-color pen-width pen-style)
           (send this set-brush brush-color brush-style)
           (send this draw-polygon-glyph v r sides start-angle)]
          ; flare (plus, asterisk, etc.)
          [(flare-glyph alpha v r sticks start-angle
                        pen-color pen-width pen-style)
           (send this set-pen pen-color pen-width pen-style)
           (send this draw-flare-glyph v r sticks start-angle)]
          ; arrow
          [(arrow-glyph alpha v r angle pen-color pen-width pen-style)
           (send this set-pen pen-color pen-width pen-style)
           (send this draw-arrow-glyph v r angle)]
          [_  (error 'end-plot "shape not implemented: ~e" s)])))
    
    (define/public (add-line/center v1 v2 c)
      (let-values ([(v1 v2)  (if clipping?
                                 (clip-line v1 v2 clip-x-min clip-x-max
                                            clip-y-min clip-y-max
                                            clip-z-min clip-z-max)
                                 (values v1 v2))])
        (when (and v1 v2)
          (add-shape! (line (send this get-alpha) (plot->view c)
                            (plot->view v1) (plot->view v2)
                            (send this get-pen-color)
                            (send this get-pen-width)
                            (send this get-pen-style))))))
    
    (define/public (add-line v1 v2)
      (add-line/center v1 v2 (center-coord (list v1 v2))))
    
    (define/public (add-lines vs)
      (for ([v1  (in-list vs)] [v2  (in-list (rest vs))])
        (add-line v1 v2)))
    
    (define/public (add-polygon/center vs c)
      (let* ([vs  (if clipping?
                      (clip-polygon vs clip-x-min clip-x-max
                                    clip-y-min clip-y-max
                                    clip-z-min clip-z-max)
                      vs)]
             [vs  (map plot->view vs)])
        (when (not (empty? vs))
          (add-shape! (polygon (send this get-alpha) (plot->view c)
                               vs
                               (send this get-pen-color)
                               (send this get-pen-width)
                               (send this get-pen-style)
                               (send this get-brush-color)
                               (send this get-brush-style))))))
    
    (define/public (add-polygon vs)
      (add-polygon/center vs (center-coord vs)))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max
                            clip-y-min clip-y-max
                            clip-z-min clip-z-max)))
    
    (define/public (add-text str v [anchor 'center] [angle 0])
      (when (in-bounds? v)
        (add-shape! (text (send this get-alpha) (plot->view v)
                          anchor angle str
                          (send this get-font-size)
                          (send this get-font-family)
                          (send this get-text-foreground)))))
    
    (define/public (add-point v)
      (when (in-bounds? v)
        (add-shape! (point (send this get-alpha) (plot->view v)
                           (send this get-pen-color)
                           (send this get-pen-width)
                           (send this get-pen-style)))))
    
    (define/public (add-circle-glyph v r)
      (when (in-bounds? v)
        (add-shape! (circle-glyph (send this get-alpha) (plot->view v)
                                  r
                                  (send this get-pen-color)
                                  (send this get-pen-width)
                                  (send this get-pen-style)
                                  (send this get-brush-color)
                                  (send this get-brush-style)))))
    
    (define/public (add-polygon-glyph v r sides start-angle)
      (when (in-bounds? v)
        (add-shape! (polygon-glyph (send this get-alpha) (plot->view v)
                                   r sides start-angle
                                   (send this get-pen-color)
                                   (send this get-pen-width)
                                   (send this get-pen-style)
                                   (send this get-brush-color)
                                   (send this get-brush-style)))))
    
    (define/public (add-flare-glyph v r sticks start-angle)
      (when (in-bounds? v)
        (add-shape! (flare-glyph (send this get-alpha) (plot->view v)
                                 r sticks start-angle
                                 (send this get-pen-color)
                                 (send this get-pen-width)
                                 (send this get-pen-style)))))
    
    (define/public (add-arrow-glyph v r angle)
      (when (in-bounds? v)
        (add-shape! (arrow-glyph (send this get-alpha) (plot->view v)
                                 r angle
                                 (send this get-pen-color)
                                 (send this get-pen-width)
                                 (send this get-pen-style)))))
    
    (define/public (add-text-glyph v str)
      (add-text str v))
    ))
