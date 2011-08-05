#lang racket/base

(require racket/gui racket/list
         "../common/utils.rkt"
         "../common/ticks.rkt")

(provide plot3d-samples plot3d-lighting 
         center-coord
         3d-plot-area% 3d-plot-snip%)

(define plot3d-samples (make-parameter 40))

(define plot3d-lighting (make-parameter '(diffuse specular)))

(define x-tick-major (make-parameter 2))
(define x-tick-size (make-parameter 10))

(define y-tick-major (make-parameter 2))
(define y-tick-size (make-parameter 10))

(define title-string (make-parameter "Untitled"))
(define x-axis-string (make-parameter "x axis"))
(define y-axis-string (make-parameter "y axis"))

(define fg-color (make-parameter (make-object color% 0 0 0)))
(define bg-color (make-parameter (make-object color% 255 255 255)))
(define font-size (make-parameter 8))
(define pen-width (make-parameter 1))

(define (pen-gap) (* 2 (pen-width)))

;; =============================================================================
;; Shapes and sorting

(struct shape (center) #:transparent)

(define (center-coord vs)
  (define ((ref n) v) (vector-ref v n))
  (define x-min (apply min (map (ref 0) vs)))
  (define x-max (apply max (map (ref 0) vs)))
  (define y-min (apply min (map (ref 1) vs)))
  (define y-max (apply max (map (ref 1) vs)))
  (define z-min (apply min (map (ref 2) vs)))
  (define z-max (apply max (map (ref 2) vs)))
  (vector (* 1/2 (+ x-min x-max))
          (* 1/2 (+ y-min y-max))
          (* 1/2 (+ z-min z-max))))

(struct polygon shape (pen brush vs) #:transparent)
(struct line shape (pen v1 v2) #:transparent)
(struct text shape (font text) #:transparent)

(define (shape-norm shape)
  (match shape
    [(polygon _center _pen _brush (list v1 v2 v3 vs ...))
     (vcross (v- v2 v1) (v- v3 v1))]
    [_  (vector 0 -1 0)]))

(define (shape-coords shape)
  (match shape
    [(polygon _center _pen _brush vs) vs]
    [(line _center _pen v1 v2) (list v1 v2)]
    [(text v font text) (list v)]))

(define (draw-before? s1 s2)
  (define c1 (vector-ref (shape-center s1) 1))
  (define c2 (vector-ref (shape-center s2) 1))
  (or (c1 . > . c2)
      (and (polygon? s1) (not (polygon? s2))
           (c1 . = . c2))))

(define (depth-sort shapes)
  (sort shapes draw-before?))

;; =============================================================================
;; Plot area

(define 3d-plot-area%
  (class object%
    (init-field x-min x-max y-min y-max z-min z-max theta rho dc)
    
    (define samples (plot3d-samples))
    (define xs (real-seq x-min x-max samples))
    (define ys (real-seq y-min y-max samples))
    (define zs (real-seq z-min z-max samples))
    
    (define-values (dc-x-size dc-y-size) (send dc get-size))
    
    (define x-margin
      0
      #;
      (+ (* 10 (send dc get-char-width))
         (pen-gap)
         (* 3/2 (send dc get-char-height))))
    
    (define y-margin
      0
      #;
      (+ (* 1/2 (y-tick-size))
         (pen-gap)
         (send dc get-char-height)
         (* 3/2 (send dc get-char-height))))
    
    (define area-x-size
      (- dc-x-size
         x-margin
         #;
         (max (* 5 (send dc get-char-width))
              (* 1/2 (y-tick-size)))))
    
    (define area-y-size
      (- dc-y-size
         y-margin
         #;
         (max (* 1/2 (send dc get-char-height))
              (* 1/2 (x-tick-size)))
         #;
         (pen-gap)
         #;
         (* 3/2 (send dc get-char-height))))
    
    (define/public (get-dc) dc)
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-z-min) z-min)
    (define/public (get-z-max) z-max)
    (define/public (get-area-x-size) area-x-size)
    (define/public (get-area-y-size) area-y-size)
    (define/public (get-theta) theta)
    (define/public (get-rho) rho)
    
    (define/public (plot->view/coord xyz)
      (match-define (vector x y z) xyz)
      ; translate to origin
      (let ([x  (- x (* 1/2 (+ x-min x-max)))]
            [y  (- y (* 1/2 (+ y-min y-max)))]
            [z  (- z (* 1/2 (+ z-min z-max)))])
        ; rotate theta
        (let ([x  (- (* (cos theta) x) (* (sin theta) y))]
              [y  (+ (* (sin theta) x) (* (cos theta) y))])
          ; rotate rho
          (let ([y  (- (* (cos rho) y) (* (sin rho) z))]
                [z  (+ (* (sin rho) y) (* (cos rho) z))])
            (vector x y z)))))
    
    (define corners
      (list (vector x-min y-min z-min) (vector x-min y-min z-max)
            (vector x-min y-max z-min) (vector x-min y-max z-max)
            (vector x-max y-min z-min) (vector x-max y-min z-max)
            (vector x-max y-max z-min) (vector x-max y-max z-max)))
    
    (define-values (view-x-min view-x-max view-z-min view-z-max)
      (let ()
        (define view-corners
          (map (λ (v) (send this plot->view/coord v)) corners))
        (define xs (map (λ (xyz) (vector-ref xyz 0)) view-corners))
        (define zs (map (λ (xyz) (vector-ref xyz 2)) view-corners))
        (values (apply min xs) (apply max xs) (apply min zs) (apply max zs))))
    
    (define/public (view->dc/x-size x)
      (* x (/ area-x-size (- view-x-max view-x-min))))
    
    (define/public (view->dc/y-size y)
      (* y (/ area-y-size (- view-z-max view-z-min))))
    
    (define/public (dc->view/x-size x)
      (* x (/ (- view-x-max view-x-min) area-x-size)))
    
    (define/public (dc->view/y-size y)
      (* y (/ (- view-z-max view-z-min) area-y-size)))
    
    (define (view->dc/coord xyz)
      (match-define (vector x y z) xyz)
      (vector (+ x-margin (* 1/2 area-x-size) (view->dc/x-size x))
              (- dc-y-size
                 (+ y-margin (* 1/2 area-y-size) (view->dc/y-size z)))))
    
    (define (view-coord->dc-point xyz)
      (match-define (vector x y) (view->dc/coord xyz))
      (make-object point% x y))
    
    (define (draw-borders)
      (define major-pen (get-pen))
      (define minor-pen (make-object pen%
                          (send major-pen get-color)
                          (* 1/2 (send major-pen get-width))
                          (send major-pen get-style)))
      (set-pen minor-pen)
      ; x axes
      (for ([x1  (in-list xs)]
            [x2  (in-list (rest xs))])
        (draw-line (vector x1 y-min z-min) (vector x2 y-min z-min))
        (draw-line (vector x1 y-max z-min) (vector x2 y-max z-min)))
      ; y axes
      (for ([y1  (in-list ys)]
            [y2  (in-list (rest ys))])
        (draw-line (vector x-min y1 z-min) (vector x-min y2 z-min))
        (draw-line (vector x-max y1 z-min) (vector x-max y2 z-min)))
      ; z axes
      (for ([z1  (in-list zs)]
            [z2  (in-list (rest zs))])
        (draw-line (vector x-min y-min z1) (vector x-min y-min z2))
        (draw-line (vector x-max y-min z1) (vector x-max y-min z2))
        (draw-line (vector x-max y-max z1) (vector x-max y-max z2))
        (draw-line (vector x-min y-max z1) (vector x-min y-max z2)))
      ; x ticks
      (define x-tick-half (dc->view/y-size (* 1/2 (x-tick-size))))
      (define x-tick-y-offset
        (dc->view/y-size (+ (pen-gap) (* 1/2 (x-tick-size)))))
      (draw-ticks
       x-min x-max
       (x-tick-major)
       (λ (x major?)
         (set-pen (if major? major-pen minor-pen))
         (draw-line (vector x (- y-min x-tick-half) z-min)
                    (vector x (+ y-min x-tick-half) z-min))
         (draw-line (vector x (- y-max x-tick-half) z-min)
                    (vector x (+ y-max x-tick-half) z-min))
         #;
         (when major?
           (define x-str (format-tick-label x))
           (define str-width (get-text-width x-str))
           (draw-text x-str (vector (- x (* 1/2 str-width))
                                    (- y-min x-tick-y-offset))))))
      ; y ticks
      (define y-tick-half (dc->view/x-size (* 1/2 (y-tick-size))))
      (define y-tick-x-offset
        (dc->view/x-size (+ (pen-gap) (* 1/2 (y-tick-size)))))
      (draw-ticks
       y-min y-max
       (y-tick-major)
       (λ (y major?)
         (set-pen (if major? major-pen minor-pen))
         (draw-line (vector (- x-min y-tick-half) y z-min)
                    (vector (+ x-min y-tick-half) y z-min))
         (draw-line (vector (- x-max y-tick-half) y z-min)
                    (vector (+ x-max y-tick-half) y z-min))
         #;
         (when major?
           (define y-str (format-tick-label y))
           (define str-width (get-text-width y-str))
           (draw-text y-str (vector (- x-min (+ str-width y-tick-x-offset))
                                    (+ y (* 1/2 (get-char-height))))))))
      (set-pen major-pen))
    
    (define (clip-to-plot)
      (send dc set-clipping-rect x-margin area-x-size y-margin area-y-size))
    
    (define/public (decorate-plot)
      (draw-borders)
      ;(draw-title)
      ;(draw-x-label)
      ;(draw-y-label)
      #;(clip-to-plot))
    
    (define (get-text-width str)
      (define-values (width _1 _2 _3)
        (send dc get-text-extent str))
      (dc->view/x-size (add1 width)))
    
    (define (get-char-height)
      (dc->view/y-size (add1 (send dc get-char-height))))
    
    (define render-list empty)
    
    (define/public (start-doc)
      (set! render-list empty))
    
    (define (add-shape! shape)
      (set! render-list (cons shape render-list)))
    
    (define (light-brush brush diff spec)
      (define r (+ (* (send (send brush get-color) red) diff) spec))
      (define g (+ (* (send (send brush get-color) green) diff) spec))
      (define b (+ (* (send (send brush get-color) blue) diff) spec))
      (make-object brush%
        (make-object color%
          (float->color-byte r) (float->color-byte g) (float->color-byte b))
        (send brush get-style)))
    
    (define (light-pen pen diff spec)
      (define r (+ (* (send (send pen get-color) red) diff) spec))
      (define g (+ (* (send (send pen get-color) green) diff) spec))
      (define b (+ (* (send (send pen get-color) blue) diff) spec))
      (make-object pen%
        (make-object color%
          (float->color-byte r) (float->color-byte g) (float->color-byte b))
        (send pen get-width)
        (send pen get-style)))
    
    (define light
      (plot->view/coord
       (vector (* 1/2 (+ x-min x-max))
               (* 1/2 (+ y-min y-max))
               (+ z-max (* 10 (- z-max z-min))))))
    
    (define (get-light-values shape)
      (let/ec return
        (define diffuse? (member 'diffuse (plot3d-lighting)))
        (define specular? (member 'specular (plot3d-lighting)))
        (when (and (not diffuse?) (not specular?))
          (return 1.0 0.0))
        ; common lighting values
        (define light-dir (let ([center  (shape-center shape)])
                            (vnormalize (v- light center))))
        (define norm (vnormalize (shape-norm shape)))
        ; diffuse lighting: typical Lambertian surface model
        (define diff
          (cond [(member 'diffuse (plot3d-lighting))
                 (+ 2/3 (* 1/3 (abs (vdot norm light-dir))))]
                [else  1.0]))
        ; specular highlighting: Gaussian variant of the Blinn-Phong model
        (define spec
          (cond [(member 'specular (plot3d-lighting))
                 (define view-dir (vector 0 -1 0))
                 (define lv (vnormalize (v* (v+ light-dir view-dir) 1/2)))
                 (define angle (acos (vdot norm lv)))
                 (* 48 (exp (- (sqr (/ angle 1/4)))))]
                [else  0.0]))
        (values diff spec)))
    
    (define/public (end-doc)
      (for ([shape  (in-list (depth-sort render-list))])
        (define-values (diff spec) (get-light-values shape))
        (match shape
          [(polygon _ pen brush xyzs)
           (send dc set-pen (light-pen pen diff spec))
           (send dc set-brush (light-brush brush diff spec))
           (send dc draw-polygon (map view-coord->dc-point xyzs))]
          [(line _ pen xyz1 xyz2)
           (match-define (vector x1 y1) (view->dc/coord xyz1))
           (match-define (vector x2 y2) (view->dc/coord xyz2))
           (send dc set-pen (light-pen pen diff spec))
           (send dc draw-line x1 y1 x2 y2)]
          [_  (error 'end-doc "shape not implemented: ~e" shape)])))
    
    (define/public (draw-line v1 v2)
      (let ([v1  (plot->view/coord v1)]
            [v2  (plot->view/coord v2)])
        (add-shape! (line (center-coord (list v1 v2))
                          (send dc get-pen)
                          v1 v2))))
    
    (define/public (draw-line/center v1 v2 c)
      (let ([v1  (plot->view/coord v1)]
            [v2  (plot->view/coord v2)]
            [c   (plot->view/coord c)])
        (add-shape! (line c (send dc get-pen) v1 v2))))
    
    (define/public (draw-polygon vs)
      (add-shape! (polygon (plot->view/coord (center-coord vs))
                           (send dc get-pen) (send dc get-brush)
                           (map (λ (v) (send this plot->view/coord v)) vs))))
    
    (define/public (draw-polygon/center vs c)
      (let ([vs  (map (λ (v) (send this plot->view/coord v)) vs)]
            [c   (plot->view/coord c)])
        (add-shape! (polygon c (send dc get-pen) (send dc get-brush) vs))))
    
    (define/public set-pen
      (case-lambda
        [(pen)  (send dc set-pen pen)]
        [(color width style)  (send dc set-pen color width style)]))
    
    (define/public (get-pen) (send dc get-pen))
    
    (define/public set-brush
      (case-lambda
        [(brush)  (send dc set-brush brush)]
        [(color style)  (send dc set-brush color style)]))
    
    (define/public (get-brush) (send dc get-brush))
    
    (super-instantiate ())
    ))

(define 3d-plot-snip%
  (class image-snip%
    (init-field bm-x-size bm-y-size
                x-min x-max y-min y-max z-min z-max
                theta rho)
    
    (define bm (make-object bitmap% bm-x-size bm-y-size #f))
    (define dc (make-object bitmap-dc% bm))
    (send dc set-background (bg-color))
    (send dc clear)
    (send dc set-font (make-object font% (font-size) 'roman))
    (send dc set-pen (fg-color) (pen-width) 'solid)
    (send dc set-brush (bg-color) 'solid)
    (send dc set-smoothing 'smoothed)
    
    (define area
      (make-object 3d-plot-area%
        x-min x-max y-min y-max z-min z-max theta rho dc))
    
    (define/public (get-area) area)
    
    (super-instantiate (bm))
    (send area decorate-plot)))
