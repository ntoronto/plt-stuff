#lang racket/base

(require racket/gui
         "color.rkt" "gui.rkt" "math.rkt")

(provide plot-area%)

(define-syntax-rule (define/abstract name)
  (define/public (name . _) (error 'name "not defined")))

(define plot-area%
  (class object%
    (init-field dc)

    (super-new)

    (define/abstract view->dc)
    
    (send dc set-smoothing 'smoothed)
    (send dc set-text-mode 'transparent)
    
    ;; fields
    
    (define pen-color '(0 0 0))
    (define pen-width 1)
    (define pen-style 'solid)
    
    (define brush-color '(255 255 255))
    (define brush-style 'solid)
    
    (define background-color '(255 255 255))
    
    (define font-size 8)
    (define font-family 'roman)
    (define text-foreground '(0 0 0))
    
    (define alpha 1.0)
    
    (define/public (get-size) (send dc get-size))
    
    ;; pen attributes
    
    (define/public (set-pen color width style)
      (set! pen-color (->color color))
      (set! pen-width width)
      (set! pen-style style))
    
    (define/public (get-pen-color) pen-color)
    (define/public (get-pen-width) pen-width)
    (define/public (get-pen-style) pen-style)
    
    (define (set-dc-pen)
      (match-define (list r g b) (map real->color-byte pen-color))
      (define pen (send dc get-pen))
      (define c (send pen get-color))
      ; only change the pen if it's different (pen changes are kind of slow)
      (unless (and (= r (send c red)) (= g (send c green)) (= b (send c blue))
                   (= pen-width (send pen get-width))
                   (symbol=? pen-style (send pen get-style)))
        (send dc set-pen (make-object color% r g b) pen-width pen-style)))
    
    ;; brush attributes
    
    (define/public (set-brush color style)
      (set! brush-color (->color color))
      (set! brush-style style))
    
    (define/public (get-brush-color) brush-color)
    (define/public (get-brush-style) brush-style)
    
    (define (set-dc-brush)
      (match-define (list r g b) (map real->color-byte brush-color))
      (define brush (send dc get-brush))
      (define c (send brush get-color))
      (unless (and (= r (send c red)) (= g (send c green)) (= b (send c blue))
               (symbol=? brush-style (send brush get-style)))
        (send dc set-brush (make-object color% r g b) brush-style)))
    
    ;; alpha
    
    (define/public (set-alpha a) (set! alpha a))
    (define/public (get-alpha) alpha)
    
    (define (set-dc-alpha) (send dc set-alpha alpha))
    
    ;; background color
    
    (define/public (set-background color)
      (set! background-color (->color color)))
    (define/public (get-background) background-color)
    
    (define (set-dc-background)
      (send dc set-background (color->color% background-color)))
    
    ;; text attributes: font
    
    ; making font% objects is REALLY SLOW, so cache them
    (define font-list (make-object font-list%))
    
    (define/public (set-font-size size) (set! font-size size))
    
    (define/public (set-font size family)
      (set! font-size size)
      (set! font-family family))
    
    (define/public (get-font-size) font-size)
    (define/public (get-font-family) font-family)
    
    (define (get-font)
      (send font-list find-or-create-font
            font-size font-family 'normal 'normal))
    
    (define (set-dc-font)
      (send dc set-font (get-font)))
    
    (define/public (get-char-width)
      (set-dc-font)
      (send dc get-char-width))
    
    (define/public (get-char-height)
      (set-dc-font)
      (send dc get-char-height))
    
    (define/public (get-text-extent str)
      (send dc get-text-extent str (get-font) #t 0))
    
    ;; text attributes: color
    
    (define/public (set-text-foreground c) (set! text-foreground (->color c)))
    (define/public (get-text-foreground) text-foreground)
    
    (define (set-dc-text-foreground)
      (send dc set-text-foreground (color->color% text-foreground)))
    
    ;; clipping
    
    (define/public (clip-to-rect x-min x-max y-min y-max)
      (match-define (vector x1 y1) (view->dc (vector x-min y-max)))
      (match-define (vector x2 y2) (view->dc (vector x-max y-min)))
      (send dc set-clipping-rect x1 y1 (- x2 x1) (- y2 y1)))
    
    (define/public (clip-to-none)
      (send dc set-clipping-region #f))
    
    ;; drawing primitives
    
    (define/public (clear)
      (set-dc-background)
      (send dc clear))
    
    (define/public (draw-point v)
      (match-define (vector x y) (view->dc v))
      (set-dc-pen)
      (set-dc-alpha)
      (send dc draw-point x y))
    
    (define/public (view-coord->dc-point v)
      (match-define (vector x y) (view->dc v))
      (make-object point% x y))
    
    (define/public (draw-polygon vs)
      (set-dc-pen)
      (set-dc-brush)
      (set-dc-alpha)
      (send dc draw-polygon (map (λ (v) (view-coord->dc-point v)) vs)))
    
    (define/public (draw-lines vs)
      (set-dc-pen)
      (set-dc-alpha)
      (send dc draw-lines (map (λ (v) (view-coord->dc-point v)) vs)))
    
    (define/public (draw-line v1 v2)
      (match-define (vector x1 y1) (view->dc v1))
      (match-define (vector x2 y2) (view->dc v2))
      (set-dc-pen)
      (set-dc-alpha)
      (send dc draw-line x1 y1 x2 y2))
    
    (define/public (draw-rectangle v1 v2)
      (match-define (vector x1 y1) (view->dc v1))
      (match-define (vector x2 y2) (view->dc v2))
      (let ([x1  (min x1 x2)]
            [x2  (max x1 x2)]
            [y1  (min y1 y2)]
            [y2  (max y1 y2)])
        (set-dc-pen)
        (set-dc-brush)
        (set-dc-alpha)
        (send dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1))))
    
    (define/public (draw-text str v [anchor 'tl] [angle 0])
      (match-define (vector x y) (view->dc v))
      (set-dc-font)
      (set-dc-text-foreground)
      (set-dc-alpha)
      (draw-text/anchor dc str x y anchor #t 0 angle))
    
    (define/public (draw-text/raw str x y [anchor 'tl] [angle 0])
      (set-dc-font)
      (set-dc-text-foreground)
      (set-dc-alpha)
      (draw-text/anchor dc str x y anchor #t 0 angle))
    
    ;; drawing glyphs
    
    (define/public (draw-circle-glyph v r)
      (match-define (vector x y) (view->dc v))
      (set-dc-pen)
      (set-dc-brush)
      (set-dc-alpha)
      (send dc draw-ellipse
            (- x r -1/2) (- y r -1/2) (* 2 r) (* 2 r)))
    
    (define/public (draw-polygon-glyph v r sides start-angle)
      (match-define (vector x y) (view->dc v))
      (define angles
        (real-seq start-angle (+ start-angle (* 2 pi)) (+ 1 sides)))
      (define pts
        (for/list ([angle  (in-list angles)])
          (make-object point%
            (+ x (* (cos angle) r)) (+ y (* (sin angle) r)))))
      (set-dc-pen)
      (set-dc-brush)
      (set-dc-alpha)
      (send dc draw-polygon pts))
    
    (define/public (draw-flare-glyph v r sticks start-angle)
      (match-define (vector x y) (view->dc v))
      (define step (/ (* 2 pi) sticks))
      (define angles (build-list sticks (λ (n) (+ start-angle (* n step)))))
      (set-dc-pen)
      (set-dc-alpha)
      (for ([angle  (in-list angles)])
        (send dc draw-line x y
              (+ x (* (cos angle) r)) (+ y (* (sin angle) r)))))
    
    (define/public (draw-arrow-glyph v r angle)
      (match-define (vector x y) (view->dc v))
      (define head-r (* 4/5 r))
      (define head-angle (* 1/6 pi))
      (define head-x (+ x (* (cos angle) r)))
      (define head-y (+ y (* (sin angle) r)))
      (define tail-x (- x (* (cos angle) r)))
      (define tail-y (- y (* (sin angle) r)))
      (set-dc-pen)
      (set-dc-alpha)
      (send dc draw-line head-x head-y tail-x tail-y)
      (send dc draw-line head-x head-y
            (- head-x (* (cos (+ angle head-angle)) head-r))
            (- head-y (* (sin (+ angle head-angle)) head-r)))
      (send dc draw-line head-x head-y
            (- head-x (* (cos (- angle head-angle)) head-r))
            (- head-y (* (sin (- angle head-angle)) head-r))))
    
    (define/public (draw-text-glyph v str)
      (match-define (vector x y) (view->dc v))
      (define-values (x-size y-size _1 _2)
        (send dc get-text-extent str))
      (set-dc-font)
      (set-dc-text-foreground)
      (set-dc-alpha)
      (send dc draw-text str (- x (* 1/2 x-size)) (- y (* 1/2 y-size)) #t))
    ))
