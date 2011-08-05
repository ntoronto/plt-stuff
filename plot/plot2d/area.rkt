#lang racket/base

(require racket/gui
         "../common/ticks.rkt")

(provide (all-defined-out))

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

(define 2d-plot-area%
  (class object%
    (init-field x-min x-max y-min y-max dc)
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    
    (define-values (dc-x-size dc-y-size) (send dc get-size))
    
    (define x-margin
      (+ (* 10 (send dc get-char-width))
         (pen-gap)
         (* 3/2 (send dc get-char-height))))
    
    (define y-margin
      (+ (* 1/2 (y-tick-size))
         (pen-gap)
         (send dc get-char-height)
         (* 3/2 (send dc get-char-height))))
    
    (define area-x-size
      (- dc-x-size
         x-margin
         (max (* 5 (send dc get-char-width))
              (* 1/2 (y-tick-size)))))
    
    (define area-y-size
      (- dc-y-size
         y-margin
         (max (* 1/2 (send dc get-char-height))
              (* 1/2 (x-tick-size)))
         (pen-gap)
         (* 3/2 (send dc get-char-height))
         ))
    
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
    
    (define/public (plot->dc/coord xy)
      (match-define (vector x y) xy)
      (define x-zero (plot->dc/x-size (- x-min)))
      (define y-zero (plot->dc/y-size (- y-min)))
      (vector (+ x-margin (+ x-zero (plot->dc/x-size x)))
              (- dc-y-size (+ y-margin (+ y-zero (plot->dc/y-size y))))))
    
    (define (plot-coord->dc-point xy)
      (match-define (vector x y) (plot->dc/coord xy))
      (make-object point% x y))
    
    (define (get-text-width str)
      (define-values (width _1 _2 _3)
        (send dc get-text-extent str))
      (dc->plot/x-size (add1 width)))
    
    (define (get-char-height)
      (dc->plot/y-size (add1 (send dc get-char-height))))
    
    (define (draw-borders)
      (define major-pen (get-pen))
      (define minor-pen (make-object pen%
                          (send major-pen get-color)
                          (* 1/2 (send major-pen get-width))
                          (send major-pen get-style)))
      (set-pen minor-pen)
      (draw-line (vector x-min y-min) (vector x-min y-max))
      (draw-line (vector x-min y-max) (vector x-max y-max))
      (draw-line (vector x-max y-max) (vector x-max y-min))
      (draw-line (vector x-max y-min) (vector x-min y-min))
      ; x ticks
      (define x-tick-half (dc->plot/y-size (* 1/2 (x-tick-size))))
      (define x-tick-y-offset
        (dc->plot/y-size (+ (pen-gap) (* 1/2 (x-tick-size)))))
      (draw-ticks
       x-min x-max
       (x-tick-major)
       (λ (x major?)
         (set-pen (if major? major-pen minor-pen))
         (draw-line (vector x (- y-min x-tick-half))
                    (vector x (+ y-min x-tick-half)))
         (draw-line (vector x (- y-max x-tick-half))
                    (vector x (+ y-max x-tick-half)))
         (when major?
           (define x-str (format-tick-label x))
           (define str-width (get-text-width x-str))
           (draw-text x-str (vector (- x (* 1/2 str-width))
                                    (- y-min x-tick-y-offset))))))
      ; y ticks
      (define y-tick-half (dc->plot/x-size (* 1/2 (y-tick-size))))
      (define y-tick-x-offset
        (dc->plot/x-size (+ (pen-gap) (* 1/2 (y-tick-size)))))
      (draw-ticks
       y-min y-max
       (y-tick-major)
       (λ (y major?)
         (set-pen (if major? major-pen minor-pen))
         (draw-line (vector (- x-min y-tick-half) y)
                    (vector (+ x-min y-tick-half) y))
         (draw-line (vector (- x-max y-tick-half) y)
                    (vector (+ x-max y-tick-half) y))
         (when major?
           (define y-str (format-tick-label y))
           (define str-width (get-text-width y-str))
           (draw-text y-str (vector (- x-min (+ str-width y-tick-x-offset))
                                    (+ y (* 1/2 (get-char-height))))))))
      (set-pen major-pen))
    
    (define (draw-title)
      (define-values (title-x-size _1 _2 _3)
        (send dc get-text-extent (title-string)))
      (send dc draw-text (title-string)
            (/ (- dc-x-size title-x-size) 2) 0))
    
    (define (draw-x-label)
      (define-values (label-x-size _1 _2 _3)
        (send dc get-text-extent (x-axis-string)))
      (send dc draw-text (x-axis-string)
            (/ (- dc-x-size label-x-size) 2)
            (- dc-y-size (send dc get-char-height))))
    
    (define (draw-y-label)
      (define-values (label-y-size _1 _2 _3)
        (send dc get-text-extent (y-axis-string)))
      (send dc draw-text (y-axis-string)
            0 (+ (/ (- dc-y-size label-y-size) 2) label-y-size)
            #f 0 (/ pi 2)))
    
    (define/public (clip-to-plot)
      (match-define (vector x0 y0) (plot->dc/coord (vector x-min y-max)))
      (match-define (vector x1 y1) (plot->dc/coord (vector x-max y-min)))
      (send dc set-clipping-rect x0 y0 (- x1 x0) (- y1 y0)))
    
    (define/public (clip-to-whole)
      (send dc set-clipping-region #f))
    
    (define (draw-axes)
      ; horizontal
      (define y-tick-half-len (dc->plot/x-size (* 1/2 (y-tick-size))))
      (draw-line (vector (+ x-min y-tick-half-len) 0)
                 (vector (- x-max y-tick-half-len) 0))
      ; vertical
      (define x-tick-half-len (dc->plot/y-size (* 1/2 (x-tick-size))))
      (draw-line (vector 0 (+ y-min x-tick-half-len))
                 (vector 0 (- y-max x-tick-half-len))))
    
    (define/public (decorate-plot)
      (draw-borders)
      (draw-title)
      (draw-x-label)
      (draw-y-label)
      (clip-to-plot)
      (draw-axes))
    
    (define/public (get-dc) dc)
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-area-x-size) area-x-size)
    (define/public (get-area-y-size) area-y-size)
    
    (define/public (draw-line xy1 xy2)
      (match-let ([(vector x1 y1)  (plot->dc/coord xy1)]
                  [(vector x2 y2)  (plot->dc/coord xy2)])
        (send dc draw-line x1 y1 x2 y2)))
    
    (define/public (draw-lines xys)
      (send dc draw-lines (map plot-coord->dc-point xys)))
    
    (define/public (draw-polygon xys)
      (send dc draw-polygon (map plot-coord->dc-point xys)))
    
    (define/public (draw-text text xy [angle 0])
      (match-define (vector x y) (plot->dc/coord xy))
      (define-values (x-size y-size _1 _2)
        (send dc get-text-extent text))
      (let ([x  (if (x . < . 0)
                    (if ((+ x x-size) . > . dc-x-size) x 0)
                    (if ((+ x x-size) . > . dc-x-size) (- dc-x-size x-size) x))]
            [y  (if (y . < . 0)
                    (if ((+ y y-size) . > . dc-y-size) y 0)
                    (if ((+ y y-size) . > . dc-y-size) (- dc-y-size y-size) y))])
        (send dc draw-text text x y #t 0 angle)))
    
    (define/public (draw-text-point text xy)
      (match-define (vector x y) (plot->dc/coord xy))
      (define-values (x-size y-size _1 _2)
        (send dc get-text-extent text))
      (send dc draw-text text (- x (* 1/2 x-size)) (- y (* 1/2 y-size))))
    
    (define/public (draw-point xy)
      (match-define (vector x y) (plot->dc/coord xy))
      (send dc draw-point x y))
    
    (define/public (draw-ellipse xy xy-size)
      (match-define (vector x y) (plot->dc/coord xy))
      (match-define (vector x-size y-size) (plot->dc/size xy-size))
      (send dc draw-ellipse x (- y y-size) x-size y-size))
    
    (define/public (draw-rectangle xy xy-size)
      (match-define (vector x y) (plot->dc/coord xy))
      (match-define (vector x-size y-size) (plot->dc/size xy-size))
      (send dc draw-rectangle x (- y y-size) x-size y-size))
    
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
    
    (define/public (set-text-foreground color)
      (send dc set-text-foreground color))
    
    (super-new)))

(define 2d-plot-snip%
  (class image-snip%
    (init-field bm-x-size bm-y-size x-min x-max y-min y-max)
    
    (define bm (make-object bitmap% bm-x-size bm-y-size #f))
    (define dc (make-object bitmap-dc% bm))
    (send dc set-background (bg-color))
    (send dc clear)
    (send dc set-font (make-object font% (font-size) 'roman))
    (send dc set-pen (fg-color) (pen-width) 'solid)
    (send dc set-smoothing 'smoothed)
    
    (define area (make-object 2d-plot-area% x-min x-max y-min y-max dc))
    
    (define/public (get-area) area)
    
    (super-instantiate (bm))
    (send area decorate-plot)))
