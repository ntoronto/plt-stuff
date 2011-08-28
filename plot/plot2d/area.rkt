#lang racket/base

(require racket/draw racket/class racket/contract racket/match racket/math
         racket/list
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt"
         "clip.rkt")

(provide (all-defined-out))

(defparam plot2d-tick-skip skip positive-integer/c 2)
(defparam plot2d-tick-size size nonnegative-real/c 10)

(defparam plot2d-title title (or/c string? #f) #f)
(defparam plot2d-x-label label (or/c string? #f) "x axis")
(defparam plot2d-y-label label (or/c string? #f) "y axis")

(defparam plot2d-foreground color plot-color/c "black")
(defparam plot2d-background color plot-color/c "white")
(defparam plot2d-font-size size (integer-in 1 255) 11)
(defparam plot2d-font-family family font-family/c 'roman)
(defparam plot2d-pen-width width nonnegative-real/c 1)

(define (plot2d-pen-gap) (* 2 (plot2d-pen-width)))

(define 2d-plot-area%
  (class plot-area%
    (init-field x-ticks y-ticks x-min x-max y-min y-max)
    (init the-dc)
    
    (inherit set-font set-text-foreground set-pen set-brush set-background
             set-alpha get-text-width get-text-extent get-char-height get-size
             clear draw-text/raw draw-line draw-lines draw-polygon draw-text
             draw-rectangle draw-point draw-circle-glyph draw-polygon-glyph
             draw-flare-glyph draw-arrow-glyph draw-text-glyph)
    
    (super-make-object the-dc)
    
    (define (reset-drawing-params)
      (set-font (plot2d-font-size) (plot2d-font-family))
      (set-text-foreground (plot2d-foreground))
      (set-pen (plot2d-foreground) (plot2d-pen-width) 'solid)
      (set-brush (plot2d-background) 'solid)
      (set-background (plot2d-background))
      (set-alpha 1.0))
    
    (reset-drawing-params)
    
    (define max-y-tick-label-width
      (for/fold ([max-w 0]) ([t  (in-list y-ticks)])
        (define-values (w h _1 _2)  (get-text-extent (tick-label t)))
        (values (max max-w w))))
    
    (define char-height (get-char-height))
    
    (define last-x-tick-label-width
      (let-values ([(w h _1 _2)  (get-text-extent
                                  (tick-label (argmax tick-p x-ticks)))])
        w))
    
    (define-values (dc-x-size dc-y-size) (get-size))
      
    (define x-margin
      (+ (* 1/2 (plot2d-tick-size))                   ; y ticks
         (plot2d-pen-gap) max-y-tick-label-width      ; y tick labels
         (if (plot2d-y-label) (* 3/2 char-height) 0)  ; y label
         ))  
    
    (define area-x-size
      (- dc-x-size x-margin
         (max (* 1/2 last-x-tick-label-width)  ; protruding x tick label
              (* 1/2 (plot2d-tick-size)))      ; y ticks
         ))
    
    (define y-margin
      (+ (* 1/2 (plot2d-tick-size))                   ; x ticks
         (plot2d-pen-gap) char-height                 ; x tick labels
         (if (plot2d-x-label) (* 3/2 char-height) 0)  ; x label
         ))
    
    (define area-y-size
      (- dc-y-size y-margin
         (max (* 1/2 char-height)                   ; protruding y tick label
              (* 1/2 (plot2d-tick-size)))           ; x ticks
         (if (plot2d-title) (* 3/2 char-height) 0)  ; title
         ))
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    
    (define/public (clip-to-bounds rx-min rx-max ry-min ry-max)
      (set! clipping? #t)
      (define cx-min (if rx-min (max x-min rx-min) x-min))
      (define cx-max (if rx-max (min x-max rx-max) x-max))
      (define cy-min (if ry-min (max y-min ry-min) y-min))
      (define cy-max (if ry-max (min y-max ry-max) y-max))
      (let ([cx-min  (min cx-min cx-max)]
            [cx-max  (max cx-min cx-max)]
            [cy-min  (min cy-min cy-max)]
            [cy-max  (max cy-min cy-max)])
        (set! clip-x-min cx-min)
        (set! clip-x-max cx-max)
        (set! clip-y-min cy-min)
        (set! clip-y-max cy-max)))
    
    (define/public (clip-to-none)
      (set! clipping? #f))
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    
    (define/public (dc->plot/x-size x)
      (* x (/ x-size area-x-size)))
    
    (define/public (dc->plot/y-size y)
      (* y (/ y-size area-y-size)))
    
    (define/public (plot->dc/x-size x)
      (* x (/ area-x-size x-size)))
    
    (define/public (plot->dc/y-size y)
      (* y (/ area-y-size y-size)))
    
    (define/public (plot->dc/size xy)
      (match-define (vector x y) xy)
      (vector (plot->dc/x-size x) (plot->dc/y-size y)))
    
    (define (plot->dc xy)
      (match-define (vector x y) xy)
      (define x-zero (plot->dc/x-size (- x-min)))
      (define y-zero (plot->dc/y-size (- y-min)))
      (vector (+ x-margin (+ x-zero (plot->dc/x-size x)))
              (- dc-y-size (+ y-margin (+ y-zero (plot->dc/y-size y))))))
    
    ; in 2D, plot coordinates are view coordinates
    (define/override (view->dc xy) (plot->dc xy))
    
    (define (set-major-pen)
      (set-pen (plot2d-foreground) (plot2d-pen-width) 'solid))
    
    (define (set-minor-pen)
      (set-pen (plot2d-foreground) (* 1/2 (plot2d-pen-width)) 'solid))
    
    (define/private (add-borders)
      (set-minor-pen)
      (add-line (vector x-min y-min) (vector x-min y-max))
      (add-line (vector x-min y-max) (vector x-max y-max))
      (add-line (vector x-max y-max) (vector x-max y-min))
      (add-line (vector x-max y-min) (vector x-min y-min)))
    
    (define/private (add-axes)
      (clip-to-bounds x-min x-max y-min y-max)
      (set-minor-pen)
      ; horizontal
      (define y-tick-half (dc->plot/x-size (* 1/2 (plot2d-tick-size))))
      (add-line (vector (+ x-min y-tick-half) 0)
                (vector (- x-max y-tick-half) 0))
      ; vertical
      (define x-tick-half (dc->plot/y-size (* 1/2 (plot2d-tick-size))))
      (add-line (vector 0 (+ y-min x-tick-half))
                (vector 0 (- y-max x-tick-half))))
    
    (define/private (add-x-ticks)
      (define half (dc->plot/y-size (* 1/2 (plot2d-tick-size))))
      (define y-offset
        (dc->plot/y-size (+ (plot2d-pen-gap) (* 1/2 (plot2d-tick-size)))))
      (for ([t  (in-list x-ticks)])
        (match-define (tick x x-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (add-line (vector x (- y-min half))
                  (vector x (+ y-min half)))
        (add-line (vector x (- y-max half))
                  (vector x (+ y-max half)))
        (when major?
          (define str-width (dc->plot/x-size (get-text-width x-str)))
          (add-text x-str (vector (- x (* 1/2 str-width))
                                  (- y-min y-offset))))))
    
    (define/private (add-y-ticks)
      (define half (dc->plot/x-size (* 1/2 (plot2d-tick-size))))
      (define x-offset
        (dc->plot/x-size (+ (plot2d-pen-gap) (* 1/2 (plot2d-tick-size)))))
      (define y-offset (* 1/2 (dc->plot/y-size char-height)))
      (for ([t  (in-list y-ticks)])
        (match-define (tick y y-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (add-line (vector (- x-min half) y)
                  (vector (+ x-min half) y))
        (add-line (vector (- x-max half) y)
                  (vector (+ x-max half) y))
        (when major?
          (define str-width (dc->plot/x-size (get-text-width y-str)))
          (add-text y-str (vector (- x-min (+ str-width x-offset))
                                  (+ y y-offset))))))
    
    (define/private (add-title)
      (define-values (title-x-size _1 _2 _3)
        (get-text-extent (plot2d-title)))
      (draw-text/raw (plot2d-title) (/ dc-x-size 2) 0 'top))
    
    (define/private (add-x-label)
      (match-define (vector x _)
        (plot->dc (vector (* 1/2 (+ x-min x-max)) 0)))
      (draw-text/raw (plot2d-x-label) x dc-y-size 'bottom))
    
    (define/private (add-y-label)
      (match-define (vector _ y)
        (plot->dc (vector 0 (* 1/2 (+ y-min y-max)))))
      (draw-text/raw (plot2d-y-label) 0 y 'bottom (/ pi -2)))
    
    (define/public (start-plot)
      (clear)
      (add-borders)
      (add-x-ticks)
      (add-y-ticks)
      (add-axes))
    
    (define/public (end-plot)
      (reset-drawing-params)
      (clip-to-none)
      (when (plot2d-title) (add-title))
      (when (plot2d-x-label) (add-x-label))
      (when (plot2d-y-label) (add-y-label)))
    
    (define/public (add-line v1 v2)
      (when (and (vall-regular? v1) (vall-regular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-line v1 v2 clip-x-min clip-x-max
                                              clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (draw-line v1 v2)))))
    
    (define/public (add-lines vs)
      (for ([vs  (vregular-sublists vs)])
        (for ([vs  (if clipping?
                       (in-list (clip-lines vs clip-x-min clip-x-max
                                            clip-y-min clip-y-max))
                       (in-value vs))])
          (when (not (empty? vs))
            (draw-lines vs)))))
    
    (define/public (add-polygon vs)
      (when (andmap vall-regular? vs)
        (let* ([vs  (if clipping?
                        (clip-polygon vs clip-x-min clip-x-max
                                      clip-y-min clip-y-max)
                        vs)])
          (when (not (empty? vs))
            (draw-polygon vs)))))
    
    (define/public (add-rectangle v1 v2)
      (when (and (vall-regular? v1) (vall-regular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-rectangle v1 v2 clip-x-min clip-x-max
                                                   clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (draw-rectangle v1 v2)))))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max
                            clip-y-min clip-y-max)))
    
    (define/public (add-text str v [anchor 'tl] [angle 0])
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-text str v anchor angle)))
    
    (define/public (add-point v)
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-point v)))
    
    (define/public (add-circle-glyph v r)
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-circle-glyph v r)))
    
    (define/public (add-polygon-glyph v r sides start-angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-polygon-glyph v r sides start-angle)))
    
    (define/public (add-flare-glyph v r sticks start-angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-flare-glyph v r sticks start-angle)))
    
    (define/public (add-arrow-glyph v r angle)
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-arrow-glyph v r angle)))
    
    (define/public (add-text-glyph v str)
      (when (and (vall-regular? v) (in-bounds? v))
        (draw-text-glyph v str)))
    ))
