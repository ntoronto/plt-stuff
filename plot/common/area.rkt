#lang racket/base

(require racket/draw racket/class racket/match racket/math racket/bool racket/list racket/contract
         "contract.rkt"
         "draw.rkt"
         "math.rkt"
         "point.rkt")

(provide (all-defined-out))

(defparam plot-foreground color plot-color/c 0)
(defparam plot-background color plot-color/c 0)
(defparam plot-font-size size font-size/c 11)
(defparam plot-font-family family font-family/c 'roman)
(defparam plot-pen-width width (real>=/c 0) 1)

(defparam plot-legend-anchor anchor anchor/c 'top-right)
(defparam plot-legend-box-alpha alpha (real-in 0 1) 2/3)

(define (pen-gap) (* 2 (plot-pen-width)))

(define (coord->cons v)
  (match-define (vector x y) v)
  (cons x y))

(define plot-area%
  (class object%
    (init-field dc)
    
    (super-new)
    
    (define/public (get-size)
      (define-values (x-size y-size) (send dc get-size))
      (values (inexact->exact x-size) (inexact->exact y-size)))

    ;; ===============================================================================================
    ;; Drawing parameters
    
    (define/public (reset-drawing-params)
      (send dc set-smoothing 'smoothed)
      (send dc set-text-mode 'transparent)
      (set-font (plot-font-size) (plot-font-family))
      (set-text-foreground (plot-foreground))
      (set-pen (plot-foreground) (plot-pen-width) 'solid)
      (set-brush (plot-background) 'solid)
      (set-background (plot-background))
      (set-alpha 1))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Pen, brush, alpha parameters
    
    (define/public (set-pen color width style)
      (match-define (list r g b) (map real->color-byte (->color/line color)))
      (let ([style  (->style/line style)])
        (define pen (send dc get-pen))
        (define c (send pen get-color))
        ; only change the pen if it's different (pen changes are kind of slow)
        (unless (and (= r (send c red)) (= g (send c green)) (= b (send c blue))
                     (= width (send pen get-width))
                     (eq? style (send pen get-style)))
          (send dc set-pen (make-object color% r g b) width style))))
    
    (define/public (set-brush color style)
      (match-define (list r g b) (map real->color-byte (->color/fill color)))
      (let ([style  (->style/fill style)])
        (define brush (send dc get-brush))
        (define c (send brush get-color))
        (define s (send brush get-style))
        (unless (and (eq? style s)
                     (or (eq? style 'transparent)
                         (and (= r (send c red)) (= g (send c green)) (= b (send c blue)))))
          (send dc set-brush (make-object color% r g b) style))))
    
    (define/public (set-alpha a) (send dc set-alpha a))
    
    (define/public (set-background color)
      (send dc set-background (color->color% (->color/fill color))))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Text parameters
    
    ; making font% objects is REALLY SLOW, so cache them
    (define font-list (make-object font-list%))
    
    (define/public set-font
      (case-lambda
        [(font)  (send dc set-font font)]
        [(size family)
         (define font (send font-list find-or-create-font
                            (real->font-size size) family 'normal 'normal #f 'default #t))
         (send dc set-font font)]))
    
    (define/public (set-font-size size)
      (set-font size (send (send dc get-font) get-family)))
    
    (define/public (get-char-height)
      (inexact->exact (send dc get-char-height)))
    
    (define/public (get-char-baseline)
      (define-values (_1 _2 b _3) (get-text-extent ""))
      (inexact->exact b))
    
    (define/public (get-text-extent str)
      (define-values (w h b d)
        (send dc get-text-extent str #f #t 0))
      (values (inexact->exact w) (inexact->exact h)
              (inexact->exact b) (inexact->exact d)))
    
    (define/public (get-text-width str)
      (define-values (w _1 _2 _3) (get-text-extent str))
      (inexact->exact w))
    
    (define/public (set-text-foreground color)
      (send dc set-text-foreground (color->color% (->color/line color))))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Clipping
    
    (define/public (set-clipping-rect v1 v2)
      (match-define (vector x1 y1) v1)
      (match-define (vector x2 y2) v2)
      (let ([x1  (min x1 x2)]
            [x2  (max x1 x2)]
            [y1  (min y1 y2)]
            [y2  (max y1 y2)])
        (send dc set-clipping-rect x1 y1 (- x2 x1) (- y2 y1))))
    
    (define/public (clear-clipping-rect)
      (send dc set-clipping-region #f))
    
    ;; ===============================================================================================
    ;; Drawing primitives
    
    (define/public (clear) (send dc clear))
    
    (define/public (draw-point v)
      (match-define (vector x y) v)
      (send dc draw-point x y))
    
    (define/public (draw-polygon vs [fill-style 'winding])
      (send dc draw-polygon (map coord->cons vs) 0 0 fill-style))
    
    (define/public (draw-rectangle v1 v2)
      (match-define (vector x1 y1) v1)
      (match-define (vector x2 y2) v2)
      (draw-polygon
       (list (vector x1 y1) (vector x1 y2) (vector x2 y2) (vector x2 y1))))
    
    (define/public (draw-lines vs)
      (send dc draw-lines (map coord->cons vs)))
    
    (define/public (draw-line v1 v2)
      (match-define (vector x1 y1) v1)
      (match-define (vector x2 y2) v2)
      (send dc draw-line x1 y1 x2 y2))
    
    (define/public (draw-text str v [anchor 'top-left] [angle 0] #:outline? [outline? #f])
      (match-define (vector x y) v)
      
      (when outline?
        (define alpha (send dc get-alpha))
        (define fg (send dc get-text-foreground))
        
        (send dc set-alpha (alpha-expt alpha 1/8))
        (send dc set-text-foreground (send dc get-background))
        (for* ([dx  (list -1 0 1)]
               [dy  (list -1 0 1)]
               #:when (not (and (zero? dx) (zero? dy))))
          (draw-text/anchor dc str (+ x dx) (+ y dy) anchor #t 0 angle))
        (send dc set-alpha alpha)
        (send dc set-text-foreground fg))
      
      (draw-text/anchor dc str x y anchor #t 0 angle))
    
    (define/public (get-text-corners str v [anchor 'top-left] [angle 0])
      (match-define (vector x y) v)
      (get-text-corners/anchor dc str x y anchor #t 0 angle))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Glyph (point symbol) primitives
    
    (define/public ((make-draw-circle-glyph r) v)
      (match-define (vector x y) v)
      (send dc draw-ellipse (- x r -1/2) (- y r -1/2) (* 2 r) (* 2 r)))
    
    (define/public (make-draw-polygon-glyph r sides start-angle)
      (define angles (linear-seq start-angle (+ start-angle (* 2 pi)) (+ 1 sides)))
      (λ (v)
        (match-define (vector x y) v)
        (send dc draw-polygon (map (λ (a) (cons (+ x (* (cos a) r)) (+ y (* (sin a) r))))
                                   angles))))
    
    (define/public (make-draw-star-glyph r sides start-angle)
      (define angles (linear-seq start-angle (+ start-angle (* 2 pi)) (+ 1 (* 2 sides))))
      (λ (v)
        (match-define (vector x y) v)
        (define pts
          (for/list ([a  (in-list angles)] [i  (in-naturals)])
            (define r-cos-a (* r (cos a)))
            (define r-sin-a (* r (sin a)))
            (cond [(odd? i)  (cons (+ x r-cos-a) (+ y r-sin-a))]
                  [else      (cons (+ x (* 1/2 r-cos-a)) (+ y (* 1/2 r-sin-a)))])))
        (send dc draw-polygon pts)))
    
    (define/public (make-draw-flare-glyph r sticks start-angle)
      (define step (/ (* 2 pi) sticks))
      (define angles (build-list sticks (λ (n) (+ start-angle (* n step)))))
      (λ (v)
        (match-define (vector x y) v)
        (for ([a  (in-list angles)])
          (send dc draw-line x y (+ x (* (cos a) r)) (+ y (* (sin a) r))))))
    
    (define/public (make-draw-tick r angle)
      (define dx (* (cos angle) r))
      (define dy (* (sin angle) r))
      (λ (v)
        (match-define (vector x y) v)
        (send dc draw-line (- x dx) (- y dy) (+ x dx) (+ y dy))))
    
    (define/public (draw-tick v r angle)
      ((make-draw-tick r angle) v))
    
    (define/public (make-draw-arrow-glyph r angle)
      (define head-r (* 4/5 r))
      (define head-angle (* 1/6 pi))
      (define dx (* (cos angle) r))
      (define dy (* (sin angle) r))
      (define dx1 (* (cos (+ angle head-angle)) head-r))
      (define dy1 (* (sin (+ angle head-angle)) head-r))
      (define dx2 (* (cos (- angle head-angle)) head-r))
      (define dy2 (* (sin (- angle head-angle)) head-r))
      (λ (v)
        (match-define (vector x y) v)
        (define head-x (+ x dx))
        (define head-y (+ y dy))
        (define tail-x (- x dx))
        (define tail-y (- y dy))
        (send dc draw-line head-x head-y tail-x tail-y)
        (send dc draw-line head-x head-y (- head-x dx1) (- head-y dy1))
        (send dc draw-line head-x head-y (- head-x dx2) (- head-y dy2))))
    
    (define/public (draw-arrow-glyph v r angle)
      ((make-draw-arrow-glyph r angle) v))
    
    (define/public ((make-draw-text-glyph str) v)
      (match-define (vector x y) v)
      (define-values (x-size y-size _1 _2) (send dc get-text-extent str))
      (define dx (* 1/2 x-size))
      (define dy (* 1/2 y-size))
      (λ (v) (send dc draw-text str (- x dx) (- y dy) #t)))
    
    (define/public (draw-glyphs vs symbol size)
      (define pen (send dc get-pen))
      (define color (send pen get-color))
      (define width (send pen get-width))
      (define style (send pen get-style))
      (define draw-glyph
        (match symbol
          [(? string?)
           (define font (send dc get-font))
           (set-font size (send font get-family))
           (set-text-foreground color)
           (make-draw-text-glyph symbol)]
          [(? symbol?)
           (define r (* 1/2 size))
           (define new-symbol
             (cond [(hash-has-key? full-glyph-hash symbol)  (set-pen color width 'transparent)
                                                            (set-brush color 'solid)
                                                            (hash-ref full-glyph-hash symbol)]
                   [else  (set-pen color width 'solid)
                          (set-brush color 'transparent)
                          symbol]))
           (case new-symbol
             ; circles
             [(circle)  (make-draw-circle-glyph r)]
             ; squares
             [(square)   (make-draw-polygon-glyph r 4 (* 1/4 pi))]
             [(diamond)  (make-draw-polygon-glyph r 4 0)]
             ; triangles
             [(triangle
               triangleup)     (make-draw-polygon-glyph r 3 (* -1/2 pi))]
             [(triangledown)   (make-draw-polygon-glyph r 3 (* 1/2 pi))]
             [(triangleleft)   (make-draw-polygon-glyph r 3 pi)]
             [(triangleright)  (make-draw-polygon-glyph r 3 0)]
             ; dots
             [(point pixel dot)  (set-pen color (* 1/2 r) 'solid)
                                 (λ (v) (draw-point v))]
             ; flares
             [(plus)        (make-draw-flare-glyph r 4 0)]
             [(times)       (make-draw-flare-glyph r 4 (* 1/4 pi))]
             [(5asterisk)   (make-draw-flare-glyph r 5 (* -1/2 pi))]
             [(asterisk)    (make-draw-flare-glyph r 6 (* -1/2 pi))]
             [(oplus)       (make-draw-circle-glyph (+ width r))
                            (make-draw-flare-glyph r 4 0)]
             [(otimes)      (make-draw-circle-glyph (+ width r))
                            (make-draw-flare-glyph r 4 (* 1/4 pi))]
             [(o5asterisk)  (make-draw-circle-glyph (+ width r))
                            (make-draw-flare-glyph r 5 (* -1/2 pi))]
             [(oasterisk)   (make-draw-circle-glyph (+ width r))
                            (make-draw-flare-glyph r 6 (* -1/2 pi))]
             ; arrows
             [(rightarrow)  (make-draw-arrow-glyph (+ 1 r) 0)]
             [(leftarrow)   (make-draw-arrow-glyph (+ 1 r) pi)]
             [(uparrow)     (make-draw-arrow-glyph (+ 1 r) (* -1/2 pi))]
             [(downarrow)   (make-draw-arrow-glyph (+ 1 r) (* 1/2 pi))]
             ; stars
             [(3star)       (make-draw-star-glyph (+ 1 r) 3 (* 1/2 pi))]
             [(4star)       (make-draw-star-glyph (+ 1 r) 4 (* 1/2 pi))]
             [(5star)       (make-draw-star-glyph (+ 1 r) 5 (* 1/2 pi))]
             [(6star)       (make-draw-star-glyph (+ 1 r) 6 (* 1/2 pi))]
             [(7star)       (make-draw-star-glyph (+ 1 r) 7 (* 1/2 pi))]
             [(8star)       (make-draw-star-glyph (+ 1 r) 8 (* 1/2 pi))]
             [else  (raise-type-error 'draw-glyphs
                                      (format "one of ~a" known-point-symbols)
                                      symbol)])]
        [_  (raise-type-error 'draw-glyphs "string or symbol" symbol)]))
      
      (for ([v  (in-list vs)])
        (draw-glyph v)))
    
    ;; ===============================================================================================
    ;; Legend
    
    (define/public (draw-legend legend-entries x-min x-max y-min y-max)
      (define n (length legend-entries))
      (match-define (list (legend-entry labels draws) ...) legend-entries)
      
      (define-values (_1 label-y-size baseline _2) (get-text-extent (first labels)))
      (define horiz-gap (get-text-width " "))
      (define top-gap baseline)
      (define bottom-gap (* 1/2 baseline))
      (define baseline-skip (+ label-y-size baseline))
      
      (define max-label-x-size (apply max (map (λ (label) (get-text-width label)) labels)))
      (define labels-x-size (+ max-label-x-size horiz-gap))
      
      (define draw-y-size (- label-y-size baseline))
      (define draw-x-size (* 4 draw-y-size))
      
      (define legend-x-size (+ horiz-gap
                               labels-x-size (* 2 horiz-gap)
                               draw-x-size horiz-gap))
      (define legend-y-size (+ top-gap (* n baseline-skip) bottom-gap))
      
      (define legend-x-min
        (case (plot-legend-anchor)
          [(top-left left bottom-left)     x-min]
          [(top-right right bottom-right)  (- x-max legend-x-size)]
          [(center bottom top)             (- (* 1/2 (+ x-min x-max))
                                              (* 1/2 legend-x-size))]))
      
      (define legend-y-min
        (case (plot-legend-anchor)
          [(top-left top top-right)           y-min]
          [(bottom-left bottom bottom-right)  (- y-max legend-y-size)]
          [(center left right)                (- (* 1/2 (+ y-min y-max))
                                                 (* 1/2 legend-y-size))]))
      
      (define legend-x-max (+ legend-x-min legend-x-size))
      (define legend-y-max (+ legend-y-min legend-y-size))
      
      (define label-x-min (+ legend-x-min horiz-gap))
      (define draw-x-min (+ legend-x-min (* 2 horiz-gap) labels-x-size horiz-gap))
      (define draw-x-max (+ draw-x-min draw-x-size))
      
      (set-alpha (plot-legend-box-alpha))
      (set-pen (plot-foreground) (* 1/2 (plot-pen-width)) 'solid)
      (set-brush (plot-background) 'solid)
      (draw-rectangle (vector legend-x-min legend-y-min) (vector legend-x-max legend-y-max))
      
      (set-clipping-rect (vector legend-x-min legend-y-min) (vector legend-x-max legend-y-max))
      (for ([label  (in-list labels)]
            [draw   (in-list draws)]
            [i      (in-naturals)])
        (define label-y-min (+ legend-y-min top-gap (* i baseline-skip)))
        (define draw-y-min (+ label-y-min (* 1/2 baseline)))
        (define draw-y-max (+ draw-y-min draw-y-size))
        
        (reset-drawing-params)
        (draw-text label (vector label-x-min label-y-min) #:outline? #t)
        (draw this draw-x-min draw-x-max draw-y-min draw-y-max))
      
      (clear-clipping-rect))
    ))  ; end class

(define-struct/contract legend-entry
  ([label string?]
   [draw ((is-a?/c plot-area%) real? real? real? real? . -> . void?)])
  #:transparent)
