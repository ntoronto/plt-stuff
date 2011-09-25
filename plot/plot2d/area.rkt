#lang racket/base

(require racket/draw racket/class racket/contract racket/match racket/math racket/list
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/transform.rkt"
         "../common/legend.rkt"
         "clip.rkt"
         "sample.rkt")

(provide (all-defined-out))

(defparam plot2d-tick-size (real>=/c 0) 10)

(defparam plot2d-title (or/c string? #f) #f)
(defparam plot2d-x-label (or/c string? #f) #f)
(defparam plot2d-y-label (or/c string? #f) #f)

(defparam plot2d-x-scaling (one-of/c 'linear 'log) 'linear)
(defparam plot2d-y-scaling (one-of/c 'linear 'log) 'linear)

(define 2d-plot-area%
  (class plot-area%
    (init-field x-ticks y-ticks x-min x-max y-min y-max)
    (init dc)
    (inherit
      set-alpha set-pen set-brush set-background set-font set-text-foreground
      get-text-width get-text-extent get-char-height
      get-size set-clipping-rect clear-clipping-rect reset-drawing-params
      clear draw-polygon draw-rectangle draw-line draw-lines draw-text
      draw-glyphs draw-arrow-glyph draw-legend)
    
    (super-make-object dc)
    
    (reset-drawing-params)
    
    (define max-y-tick-label-width
      (for/fold ([max-w 0]) ([t  (in-list y-ticks)])
        (cond [(tick-major? t)  (define-values (w h _1 _2)
                                  (get-text-extent (tick-label t)))
                                (max max-w w)]
              [else  max-w])))
    
    (define char-height (get-char-height))
    
    (define last-x-tick (argmax tick-p x-ticks))
    (define last-x-tick-label-width
      (cond [(tick-major? last-x-tick)  (define-values (w _1 _2 _3)
                                          (get-text-extent
                                           (tick-label last-x-tick)))
                                        w]
            [else  0]))
    
    (define-values (dc-x-size dc-y-size) (get-size))
    
    (define x-margin
      (+ (* 1/2 (plot2d-tick-size))                   ; y ticks
         (pen-gap) max-y-tick-label-width             ; y tick labels
         (if (plot2d-y-label) (* 3/2 char-height) 0)  ; y label
         ))
    
    (define area-x-size
      (- dc-x-size x-margin
         (max (* 1/2 last-x-tick-label-width)  ; protruding x tick label
              (* 1/2 (plot2d-tick-size)))      ; y ticks
         ))
    
    (define y-margin
      (+ (* 1/2 (plot2d-tick-size))                   ; x ticks
         (pen-gap) char-height                 ; x tick labels
         (if (plot2d-x-label) (* 3/2 char-height) 0)  ; x label
         ))
    
    (define area-y-size
      (- dc-y-size y-margin
         (max (* 1/2 char-height)                   ; protruding y tick label
              (* 1/2 (plot2d-tick-size)))           ; x ticks
         (if (plot2d-title) (* 3/2 char-height) 0)  ; title
         ))
    
    (define area-x-min x-margin)
    (define area-x-max (+ x-margin area-x-size))
    (define area-y-max (- dc-y-size y-margin))
    (define area-y-min (- area-y-max area-y-size))
    (define area-x-mid (* 1/2 (+ area-x-min area-x-max)))
    (define area-y-mid (* 1/2 (+ area-y-min area-y-max)))
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    
    (define/public (clip-to-bounds rx-min rx-max ry-min ry-max)
      (set! clipping? #t)
      (define cx-min (if rx-min (max* x-min rx-min) x-min))
      (define cx-max (if rx-max (min* x-max rx-max) x-max))
      (define cy-min (if ry-min (max* y-min ry-min) y-min))
      (define cy-max (if ry-max (min* y-max ry-max) y-max))
      (let ([cx-min  (min* cx-min cx-max)]
            [cx-max  (max* cx-min cx-max)]
            [cy-min  (min* cy-min cy-max)]
            [cy-max  (max* cy-min cy-max)])
        (set! clip-x-min cx-min)
        (set! clip-x-max cx-max)
        (set! clip-y-min cy-min)
        (set! clip-y-max cy-max)))
    
    (define/public (clip-to-none)
      (set! clipping? #f))
    
    (define/public (get-x-ticks) x-ticks)
    (define/public (get-y-ticks) y-ticks)
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-bounds) (values x-min x-max y-min y-max))
    
    (define/public (get-area-x-min) area-x-min)
    (define/public (get-area-x-max) area-x-max)
    (define/public (get-area-y-min) area-y-min)
    (define/public (get-area-y-max) area-y-max)
    (define/public (get-clip-x-min) (if clipping? clip-x-min x-min))
    (define/public (get-clip-x-max) (if clipping? clip-x-max x-max))
    (define/public (get-clip-y-min) (if clipping? clip-y-min y-min))
    (define/public (get-clip-y-max) (if clipping? clip-y-max y-max))
    
    (define/public (view->dc/x-size x)
      (* x (/ area-x-size x-size)))
    
    (define/public (view->dc/y-size y)
      (* y (/ area-y-size y-size)))
    
    (define/public (view->dc/angle a)
      (- (atan (view->dc/y-size (sin a))
               (view->dc/x-size (cos a)))))
    
    (define/public (view->dc/angle+mag a m)
      (define dx (view->dc/x-size (* m (cos a))))
      (define dy (view->dc/y-size (* m (sin a))))
      (values (atan (- dy) dx) (sqrt (+ (sqr dx) (sqr dy)))))
    
    (define/public (dc->view/x-size x)
      (* x (/ x-size area-x-size)))
    
    (define/public (dc->view/y-size y)
      (* y (/ y-size area-y-size)))
    
    (define/public (dc->view/angle a)
      (- (atan (dc->view/y-size (sin a))
               (dc->view/x-size (cos a)))))
    
    (define x-zero (view->dc/x-size (- x-min)))
    (define y-zero (view->dc/y-size (- y-min)))
    
    (define/public (view->dc xy)
      (match-define (vector x y) xy)
      (vector (+ area-x-min (+ x-zero (view->dc/x-size x)))
              (- area-y-max (+ y-zero (view->dc/y-size y)))))
    
    (define identity-transforms?
      (and (equal? (plot2d-x-transform) id-transform)
           (equal? (plot2d-y-transform) id-transform)))
    
    (define plot->view
      (cond [identity-transforms?  (λ (v) v)]
            [else
             (match-define (invertible-fun fx _) ((plot2d-x-transform) x-min x-max))
             (match-define (invertible-fun fy _) ((plot2d-y-transform) y-min y-max))
             (λ (v)
               (match-define (vector x y) v)
               (vector (fx x) (fy y)))]))
    
    (define/public (plot->dc v)
      (view->dc (plot->view v)))
    
    (define/public (set-major-pen [style 'solid])
      (set-pen (plot-foreground) (plot-pen-width) style))
    
    (define/public (set-minor-pen [style 'solid])
      (set-pen (plot-foreground) (* 1/2 (plot-pen-width)) style))
    
    ;; -------------------------------------------------------------------------
    ;; Plot decoration
    
    (define/private (draw-borders)
      (set-minor-pen)
      (draw-rectangle (vector area-x-min area-y-min)
                      (vector area-x-max area-y-max)))
    
    (define/private (draw-x-ticks)
      (define half (* 1/2 (plot2d-tick-size)))
      (for ([t  (in-list x-ticks)])
        (match-define (tick x x-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (match-define (vector x1 _y1) (plot->dc (vector x y-min)))
        (match-define (vector x2 _y2) (plot->dc (vector x y-max)))
        (draw-line (vector x1 (- area-y-max half))
                   (vector x1 (+ area-y-max half)))
        (draw-line (vector x2 (- area-y-min half))
                   (vector x2 (+ area-y-min half)))
        (when major?
          (draw-text x-str (vector x1 (+ area-y-max half (pen-gap))) 'top))))
    
    (define/private (draw-y-ticks)
      (define half (* 1/2 (plot2d-tick-size)))
      (for ([t  (in-list y-ticks)])
        (match-define (tick y y-str major?) t)
        (match-define (vector _x1 y1) (plot->dc (vector x-min y)))
        (match-define (vector _x2 y2) (plot->dc (vector x-max y)))
        (if major? (set-major-pen) (set-minor-pen))
        (draw-line (vector (- area-x-min half) y1)
                   (vector (+ area-x-min half) y1))
        (draw-line (vector (- area-x-max half) y2)
                   (vector (+ area-x-max half) y2))
        (when major?
          (draw-text y-str (vector (- area-x-min half (pen-gap)) y1) 'right))))
    
    (define/private (draw-title)
      (define-values (title-x-size _1 _2 _3)
        (get-text-extent (plot2d-title)))
      (draw-text (plot2d-title) (vector (/ dc-x-size 2) 0) 'top))
    
    (define/private (draw-x-label)
      (match-define (vector x _)
        (view->dc (vector (* 1/2 (+ x-min x-max)) 0)))
      (draw-text (plot2d-x-label) (vector x dc-y-size) 'bottom))
    
    (define/private (draw-y-label)
      (match-define (vector _ y)
        (view->dc (vector 0 (* 1/2 (+ y-min y-max)))))
      (draw-text (plot2d-y-label) (vector 0 y) 'bottom (/ pi -2)))
    
    ;; -------------------------------------------------------------------------
    ;; Drawing
    
    (define/public (start-plot)
      (clear)
      (draw-borders)
      (draw-x-ticks)
      (draw-y-ticks))
    
    (define/public (end-plot)
      (clear-clipping-rect)
      (clip-to-none)
      (reset-drawing-params)
      (when (plot2d-title) (draw-title))
      (when (plot2d-x-label) (draw-x-label))
      (when (plot2d-y-label) (draw-y-label)))
    
    (define/public (put-legend legend-entries)
      (define gap-size (+ (pen-gap) (* 1/2 (plot2d-tick-size))))
      (draw-legend legend-entries
                   (+ area-x-min gap-size) (- area-x-max gap-size)
                   (+ area-y-min gap-size) (- area-y-max gap-size)))
    
    (define (subdivide-line v1 v2)
      (let/ec return
        (match-define (vector dc-x1 dc-y1) (plot->dc v1))
        (match-define (vector dc-x2 dc-y2) (plot->dc v2))
        (define dc-dx (- dc-x2 dc-x1))
        (define dc-dy (- dc-y2 dc-y1))
        (when (or (zero? dc-dx) (zero? dc-dy)) (return (list v1 v2)))
        
        (match-define (vector x1 y1) v1)
        (match-define (vector x2 y2) v2)
        (cond [((abs dc-dx) . > . (abs dc-dy))
               (define num (+ 1 (inexact->exact (ceiling (* 1/3 (abs dc-dx))))))
               (define xs (nonlinear-seq x1 x2 num (plot2d-x-transform)))
               (define m (/ (- y2 y1) (- x2 x1)))
               (define b (- y1 (* m x1)))
               (define ys (map (λ (x) (+ (* m x) b)) xs))
               (map vector xs ys)]
              [else
               (define num (+ 1 (inexact->exact (ceiling (* 1/3 (abs dc-dy))))))
               (define ys (nonlinear-seq y1 y2 num (plot2d-y-transform)))
               (define m (/ (- x2 x1) (- y2 y1)))
               (define b (- x1 (* m y1)))
               (define xs (map (λ (y) (+ (* m y) b)) ys))
               (map vector xs ys)])))
    
    (define (subdivide-lines vs)
      (append
       (append*
        (for/list ([v1  (in-list vs)] [v2  (in-list (rest vs))])
          (define line-vs (subdivide-line v1 v2))
          (take line-vs (sub1 (length line-vs)))))
       (list (last vs))))
    
    (define (subdivide-polygon vs)
      (subdivide-lines (append vs (list (first vs)))))
    
    (define/public (put-lines vs)
      (for ([vs  (vregular-sublists vs)])
        (for ([vs  (if clipping?
                       (in-list (clip-lines vs clip-x-min clip-x-max
                                            clip-y-min clip-y-max))
                       (in-value vs))])
          (when (not (empty? vs))
            (let ([vs  (if identity-transforms? vs (subdivide-lines vs))])
              (draw-lines (map (λ (v) (plot->dc v)) vs)))))))
    
    (define/public (put-line v1 v2)
      (when (and (vall-regular? v1) (vall-regular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-line v1 v2 clip-x-min clip-x-max
                                              clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (if identity-transforms?
                (draw-line (plot->dc v1) (plot->dc v2))
                (draw-lines (map (λ (v) (plot->dc v))
                                 (subdivide-line v1 v2))))))))
    
    (define/public (put-polygon vs)
      (when (andmap vall-regular? vs)
        (let* ([vs  (if clipping?
                        (clip-polygon vs clip-x-min clip-x-max
                                      clip-y-min clip-y-max)
                        vs)])
          (when (not (empty? vs))
            (if identity-transforms?
                (draw-polygon (map (λ (v) (plot->dc v)) vs))
                (draw-polygon (map (λ (v) (plot->dc v))
                                   (subdivide-polygon vs))))))))
    
    (define/public (put-rectangle v1 v2)
      (when (and (vall-regular? v1) (vall-regular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-rectangle v1 v2 clip-x-min clip-x-max
                                                   clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (draw-rectangle (plot->dc v1) (plot->dc v2))))))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max
                            clip-y-min clip-y-max)))
    
    (define/public (put-text str v [anchor 'top-left] [angle 0]
                             #:outline? [outline? #f])
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-text str (plot->dc v) anchor angle #:outline? outline?)))
    
    (define/public (put-glyphs vs symbol size)
      (draw-glyphs (map (λ (v) (plot->dc v))
                        (filter (λ (v) (and (vall-regular? v) (in-bounds? v)))
                                vs))
                   symbol size))
    
    (define/public (put-arrow-glyph v r angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-arrow-glyph (plot->dc v) r angle)))
    ))
