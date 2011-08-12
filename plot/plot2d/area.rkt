#lang racket/base

(require racket/gui
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/vector.rkt")

(provide (all-defined-out))

(define plot2d-tick-skip (make-parameter 2))
(define plot2d-tick-size (make-parameter 10))

(define plot2d-title (make-parameter #f))
(define plot2d-x-label (make-parameter "x axis"))
(define plot2d-y-label (make-parameter "y axis"))

(define plot2d-fg-color (make-parameter (make-object color% 0 0 0)))
(define plot2d-bg-color (make-parameter (make-object color% 255 255 255)))
(define plot2d-font-size (make-parameter 8))
(define plot2d-font-family (make-parameter 'roman))
(define plot2d-pen-width (make-parameter 1))

(define (plot2d-pen-gap) (* 2 (plot2d-pen-width)))

(define 2d-plot-area%
  (class plot-area%
    (init-field x-min x-max y-min y-max)
    (init the-dc)
    
    (super-make-object the-dc)
    
    (send this set-font (plot2d-font-size) (plot2d-font-family))
    (send this set-text-foreground (plot2d-fg-color))
    (send this set-pen (plot2d-fg-color) (plot2d-pen-width) 'solid)
    (send this set-brush (plot2d-bg-color) 'solid)
    (send this set-background (plot2d-bg-color))
    (send this set-alpha 1.0)
    
    (define-values (dc-x-size dc-y-size) (send this get-size))
    (define char-width (send this get-char-width))
    (define char-height (send this get-char-height))
    
    (define x-margin
      (+ (* 10 char-width)
         (plot2d-pen-gap)
         (* 3/2 char-height)))
    
    (define area-x-size
      (- dc-x-size
         x-margin
         (max (* 5 char-width)
              (* 1/2 (plot2d-tick-size)))))
    
    (define y-margin
      (+ (* 1/2 (plot2d-tick-size))
         (plot2d-pen-gap)
         char-height
         (* 3/2 char-height)))
    
    (define area-y-size
      (- dc-y-size
         y-margin
         (max (* 1/2 char-height)
              (* 1/2 (plot2d-tick-size)))
         (if (plot2d-title)
             (+ (plot2d-pen-gap)
                (* 3/2 char-height))
             0)))
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    
    (define/public (dc->view/x-size x)
      (* x (/ x-size area-x-size)))
    
    (define/public (dc->view/y-size y)
      (* y (/ y-size area-y-size)))
    
    (define/public (view->dc/x-size x)
      (* x (/ area-x-size x-size)))
    
    (define/public (view->dc/y-size y)
      (* y (/ area-y-size y-size)))
    
    (define/public (view->dc/size xy)
      (match-define (vector x y) xy)
      (vector (view->dc/x-size x) (view->dc/y-size y)))
    
    (define/override (view->dc xy)
      (match-define (vector x y) xy)
      (define x-zero (view->dc/x-size (- x-min)))
      (define y-zero (view->dc/y-size (- y-min)))
      (vector (+ x-margin (+ x-zero (view->dc/x-size x)))
              (- dc-y-size (+ y-margin (+ y-zero (view->dc/y-size y))))))
    
    (define (get-text-width str)
      (define-values (width _1 _2 _3)
        (send this get-text-extent str))
      (dc->view/x-size (add1 width)))
    
    (define (set-major-pen)
      (send this set-pen (plot2d-fg-color) (plot2d-pen-width) 'solid))
    
    (define (set-minor-pen)
      (send this set-pen (plot2d-fg-color) (* 1/2 (plot2d-pen-width)) 'solid))
    
    (define/private (add-borders)
      (set-minor-pen)
      (add-line (vector x-min y-min) (vector x-min y-max))
      (add-line (vector x-min y-max) (vector x-max y-max))
      (add-line (vector x-max y-max) (vector x-max y-min))
      (add-line (vector x-max y-min) (vector x-min y-min)))
    
    (define/private (add-x-ticks ticks)
      (define half (dc->view/y-size (* 1/2 (plot2d-tick-size))))
      (define y-offset
        (dc->view/y-size (+ (plot2d-pen-gap) (* 1/2 (plot2d-tick-size)))))
      (for ([t  (in-list ticks)])
        (match-define (tick x x-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (add-line (vector x (- y-min half))
                  (vector x (+ y-min half)))
        (add-line (vector x (- y-max half))
                  (vector x (+ y-max half)))
        (when major?
          (define str-width (get-text-width x-str))
          (add-text x-str (vector (- x (* 1/2 str-width))
                                  (- y-min y-offset))))))
    
    (define/private (add-x-label)
      (match-define (vector x _)
        (view->dc (vector (* 1/2 (+ x-min x-max)) 0)))
      (send this draw-text/raw (plot2d-x-label) x dc-y-size 'bottom))
    
    (define/private (add-y-ticks ticks)
      (define half (dc->view/x-size (* 1/2 (plot2d-tick-size))))
      (define x-offset
        (dc->view/x-size (+ (plot2d-pen-gap) (* 1/2 (plot2d-tick-size)))))
      (define y-offset (* 1/2 (dc->view/y-size char-height)))
      (for ([t  (in-list ticks)])
        (match-define (tick y y-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (add-line (vector (- x-min half) y)
                  (vector (+ x-min half) y))
        (add-line (vector (- x-max half) y)
                  (vector (+ x-max half) y))
        (when major?
          (define str-width (get-text-width y-str))
          (add-text y-str (vector (- x-min (+ str-width x-offset))
                                  (+ y y-offset))))))
    
    (define/private (add-y-label)
      (match-define (vector _ y)
        (view->dc (vector 0 (* 1/2 (+ y-min y-max)))))
      (send this draw-text/raw (plot2d-y-label) 0 y 'bottom (/ pi -2)))
    
    (define/private (add-title)
      (when (plot2d-title)
        (define-values (title-x-size _1 _2 _3)
          (send this get-text-extent (plot2d-title)))
        (send this draw-text/raw (plot2d-title) (/ dc-x-size 2) 0 'top)))
    
    (define/private (add-axes)
      (clip-to-bounds x-min x-max y-min y-max)
      ; horizontal
      (define y-tick-half (dc->view/x-size (* 1/2 (plot2d-tick-size))))
      (add-line (vector (+ x-min y-tick-half) 0)
                (vector (- x-max y-tick-half) 0))
      ; vertical
      (define x-tick-half (dc->view/y-size (* 1/2 (plot2d-tick-size))))
      (add-line (vector 0 (+ y-min x-tick-half))
                (vector 0 (- y-max x-tick-half))))
    
    (define/public (start-plot x-ticks y-ticks)
      (send this clear)
      (add-title)
      (add-borders)
      (add-x-ticks (x-ticks x-min x-max))
      (add-y-ticks (y-ticks y-min y-max))
      (add-x-label)
      (add-y-label)
      (add-axes))
    
    (define/public (clip-to-bounds rx-min rx-max ry-min ry-max)
      (let ([x-min  (if rx-min (max x-min rx-min) x-min)]
            [x-max  (if rx-max (min x-max rx-max) x-max)]
            [y-min  (if ry-min (max y-min ry-min) y-min)]
            [y-max  (if ry-max (min y-max ry-max) y-max)])
        (send this clip-to-rect x-min x-max y-min y-max)))
    
    (define/public (end-plot)
      (send this clip-to-none))
    
    (define/public (add-line v1 v2)
      (when (and (vreg? v1) (vreg? v2))
        (send this draw-line v1 v2)))
    
    (define/public (add-lines vs)
      (for ([vs  (vreg-sublists vs)])
        (when (not (empty? vs))
          (send this draw-lines vs))))
    
    (define/public (add-polygon vs)
      (when (andmap vreg? vs)
        (send this draw-polygon vs)))
    
    (define/public (add-text str v [anchor 'tl] [angle 0])
      (when (vreg? v)
        (send this draw-text str v anchor angle)))
    
    (define/public (add-rectangle v1 v2)
      (when (and (vreg? v1) (vreg? v2))
        (send this draw-rectangle v1 v2)))
    
    (define/public (add-point v)
      (send this draw-point v))
    
    (define/public (add-circle-glyph v r)
      (send this draw-circle-glyph v r))
    
    (define/public (add-polygon-glyph v r sides start-angle)
      (send this draw-polygon-glyph v r sides start-angle))
    
    (define/public (add-flare-glyph v r sticks start-angle)
      (send this draw-flare-glyph v r sticks start-angle))
    
    (define/public (add-arrow-glyph v r angle)
      (send this draw-arrow-glyph v r angle))
    
    (define/public (add-text-glyph v str)
      (send this draw-text-glyph v str))
    ))
