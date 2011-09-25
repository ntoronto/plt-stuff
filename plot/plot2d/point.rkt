#lang racket/base

(require racket/contract racket/class racket/match racket/math racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "../common/draw.rkt"
         "renderer.rkt"
         "ticks.rkt"
         "clip.rkt")

(provide (except-out (all-defined-out)))

(defparam point-symbol point-symbol/c 'circle)
(defparam point-color plot-color/c 0)
(defparam point-size font-size/c 6)
(defparam point-line-width (real>=/c 0) 1)
(defparam point-alpha (real-in 0 1) 1)

(defparam vector-field-samples (integer>=/c 1) 20)
(defparam vector-field-color plot-color/c 1)
(defparam vector-field-line-width (real>=/c 0) 2/3)
(defparam vector-field-line-style line-style/c 'solid)
(defparam vector-field-arrow-scale (or/c real? (one-of/c 'auto 'normalized)) 'auto)
(defparam vector-field-alpha (real-in 0 1) 1)

(defparam error-bar-width (real>=/c 0) 6)
(defparam error-bar-line-color plot-color/c 0)
(defparam error-bar-line-width (real>=/c 0) 1)
(defparam error-bar-line-style line-style/c 'solid)
(defparam error-bar-alpha (real-in 0 1) 2/3)

;; =============================================================================
;; Points (scatter plots)

(define ((points-render-fun vs symbol color size line-width alpha label) area)
  (send area set-alpha alpha)
  (send area set-pen color line-width 'solid)
  (send area put-glyphs vs symbol size)
  
  (if label (point-legend-entry label symbol color size line-width) empty))

(defproc (points [vs  (listof (vector/c real? real?))]
                 [#:x-min x-min (or/c real? #f) #f]
                 [#:x-max x-max (or/c real? #f) #f]
                 [#:y-min y-min (or/c real? #f) #f]
                 [#:y-max y-max (or/c real? #f) #f]
                 [#:symbol symbol point-symbol/c (point-symbol)]
                 [#:color color plot-color/c (point-color)]
                 [#:size size font-size/c (point-size)]
                 [#:line-width line-width (real>=/c 0) (point-line-width)]
                 [#:alpha alpha (real-in 0 1) (point-alpha)]
                 [#:label label (or/c string? #f) #f]
                 ) renderer2d?
  (let ([vs  (filter vall-regular? vs)])
    (cond
      [(empty? vs)  null-renderer2d]
      [else
       (match-define (list (vector xs ys) ...) vs)
       (let ([x-min  (if x-min x-min (apply min* xs))]
             [x-max  (if x-max x-max (apply max* xs))]
             [y-min  (if y-min y-min (apply min* ys))]
             [y-max  (if y-max y-max (apply max* ys))])
         (make-renderer2d
          (points-render-fun vs symbol color size line-width alpha label)
          default-2d-ticks-fun
          null-2d-bounds-fun
          x-min x-max y-min y-max))])))

;; =============================================================================
;; Vector fields

(define ((vector-field-render-fun
          f samples arrow-scale color line-width line-style alpha label)
         area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  
  (define xs0 (linear-seq x-min x-max samples #:start? #f #:end? #f))
  (define ys0 (linear-seq y-min y-max samples #:start? #f #:end? #f))
  
  (define-values (xs ys dxs dys angles mags)
    (for*/lists (xs ys dxs dys angles mags)
      ([x   (in-list xs0)] [y   (in-list ys0)] [dv  (in-value (f x y))]
                           #:when (vall-regular? dv))
      (match-define (vector dx dy) dv)
      (values x y dx dy (atan dy dx) (sqrt (+ (sqr dx) (sqr dy))))))
  
  (cond
    [(empty? xs)  empty]
    [else
     (define box-x-size (/ (- x-max x-min) samples))
     (define box-y-size (/ (- y-max y-min) samples))
     
     (define new-mags
       (match arrow-scale
         [(? real?)  (map (λ (mag) (* arrow-scale mag)) mags)]
         ['normalized  (define box-size (min box-x-size box-y-size))
                       (build-list (length dxs) (λ _ box-size))]
         ['auto  (define dx-max (apply max (map abs dxs)))
                 (define dy-max (apply max (map abs dys)))
                 (define scale (min (/ box-x-size dx-max)
                                    (/ box-y-size dy-max)))
                 (map (λ (mag) (* scale mag)) mags)]))
     
     (send area set-alpha alpha)
     (send area set-pen color line-width line-style)
     (for ([x      (in-list xs)]
           [y      (in-list ys)]
           [angle  (in-list angles)]
           [mag    (in-list new-mags)])
       (define-values (a m) (send area view->dc/angle+mag angle mag))
       (send area put-arrow-glyph (vector x y) (* 1/2 m) a))
     
     (if label
         (vector-field-legend-entry label color line-width line-style)
         empty)]))

(defproc (vector-field
          [f (real? real? . -> . (vector/c real? real?))]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 1) (vector-field-samples)]
          [#:arrow-scale arrow-scale (or/c real? (one-of/c 'auto 'normalized))
                         (vector-field-arrow-scale)]
          [#:color color plot-color/c (vector-field-color)]
          [#:line-width line-width (real>=/c 0) (vector-field-line-width)]
          [#:line-style line-style line-style/c (vector-field-line-style)]
          [#:alpha alpha (real-in 0 1) (vector-field-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (make-renderer2d
   (vector-field-render-fun
    f samples arrow-scale color line-width line-style alpha label)
   default-2d-ticks-fun
   null-2d-bounds-fun
   x-min x-max y-min y-max))

;; =============================================================================
;; Error bars

(define ((error-bars-render-fun xs ys hs line-color line-width line-style
                                width alpha)
         area)
  (define x-min (send area get-clip-x-min))
  (define x-max (send area get-clip-x-max))
  (define y-min (send area get-clip-y-min))
  (define y-max (send area get-clip-y-max))
  
  (define half (* 1/2 width))
  
  (send area set-alpha alpha)
  (send area set-pen line-color line-width line-style)
  (for ([x  (in-list xs)] [y  (in-list ys)] [h  (in-list hs)])
    (when (point-in-bounds? (vector x y) x-min x-max y-min y-max)
      (define v1 (vector x (- y h)))
      (define v2 (vector x (+ y h)))
      (send area put-line v1 v2)
      
      (match-define (vector dc-x1 dc-y1) (send area plot->dc v1))
      (match-define (vector dc-x2 dc-y2) (send area plot->dc v2))
      (send area draw-line
            (vector (- dc-x1 half) dc-y1)
            (vector (+ dc-x1 half) dc-y1))
      (send area draw-line
            (vector (- dc-x2 half) dc-y2)
            (vector (+ dc-x2 half) dc-y2))))
  
  empty)

(define ((error-bars-bounds-fun xs ys hs) x-min x-max y-min y-max)
  (let ([x-min  (if x-min x-min (apply min* xs))]
        [x-max  (if x-max x-max (apply max* xs))]
        [y-min  (if y-min y-min (apply min* (map - ys hs)))]
        [y-max  (if y-max y-max (apply max* (map + ys hs)))])
    (values x-min x-max y-min y-max)))

(defproc (error-bars
          [bars (listof (vector/c real? real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:line-color line-color plot-color/c (error-bar-line-color)]
          [#:line-width line-width (real>=/c 0) (error-bar-line-width)]
          [#:line-style line-style (real>=/c 0) (error-bar-line-style)]
          [#:width width (real>=/c 0) (error-bar-width)]
          [#:alpha alpha (real-in 0 1) (error-bar-alpha)]
          ) renderer2d?
  (let ([bars  (filter vall-regular? bars)])
    (cond
      [(empty? bars)  null-renderer2d]
      [else
       (match-define (list (vector xs ys hs) ...) bars)
       (make-renderer2d
        (error-bars-render-fun
         xs ys hs line-color line-width line-style width alpha)
        default-2d-ticks-fun
        (error-bars-bounds-fun xs ys hs)
        x-min x-max y-min y-max)])))
