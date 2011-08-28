#lang racket/base

(require racket/contract racket/promise racket/class racket/match racket/math
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/points.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (except-out (all-defined-out)))

(defparam point-label label point-label/c 'circle)
(defparam point-color color plot-color/c "black")
(defparam point-size size (integer-in 1 255) 6)
(defparam point-line-width width nonnegative-real/c 2/3)
(defparam point-alpha alpha (real-in 0 1) 1)

(defparam vector-field-samples samples positive-integer/c 20)
(defparam vector-field-color color plot-color/c "darkred")
(defparam vector-field-line-width width nonnegative-real/c 2/3)
(defparam vector-field-line-style style pen-style/c 'solid)
(defparam vector-field-arrow-length length
  (one-of/c 'scaled 'normalized 'real) 'scaled)
(defparam vector-field-alpha alpha (real-in 0 1) 1)

(defparam error-bar-color color plot-color/c "black")
(defparam error-bar-width width nonnegative-real/c 6)
(defparam error-bar-line-width width nonnegative-real/c 1)
(defparam error-bar-line-style style pen-style/c 'solid)
(defparam error-bar-alpha alpha (real-in 0 1) 1)

;; =============================================================================
;; Points (scatter plots)

(defproc (points [vs  (listof (vector/c real? real?))]
                 [#:x-min x-min (or/c real? #f) #f]
                 [#:x-max x-max (or/c real? #f) #f]
                 [#:y-min y-min (or/c real? #f) #f]
                 [#:y-max y-max (or/c real? #f) #f]
                 [#:label label point-label/c (point-label)]
                 [#:color color plot-color/c (point-color)]
                 [#:size size nonnegative-real/c (point-size)]
                 [#:line-width line-width nonnegative-real/c (point-line-width)]
                 [#:alpha alpha (real-in 0 1) (point-alpha)]
                 ) renderer2d?
  (define xs (delay (map (λ (v) (vector-ref v 0)) vs)))
  (define ys (delay (map (λ (v) (vector-ref v 1)) vs)))
  (let ([x-min  (if x-min x-min (apply regular-min (force xs)))]
        [x-max  (if x-max x-max (apply regular-max (force xs)))]
        [y-min  (if y-min y-min (apply regular-min (force ys)))]
        [y-max  (if y-max y-max (apply regular-max (force ys)))])
    (make-renderer2d (add-points vs label color size line-width alpha)
                     (default-range->ticks (plot2d-tick-skip))
                     (default-range->ticks (plot2d-tick-skip))
                     values
                     x-min x-max y-min y-max)))

;; =============================================================================
;; Vector fields

(define ((add-vectors f color line-width line-style arrow-length alpha) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  
  (define samples (vector-field-samples))
  (define-values (xs ys dvs)
    (for*/lists (xs ys dvs) ([x   (in-list (real-seq x-min x-max samples))]
                             [y   (in-list (real-seq y-min y-max samples))]
                             [dv  (in-value (f x y))]
                             #:when (vall-regular? dv))
      (values x y dv)))
  
  (define dxs (map (λ (v) (exact->inexact (vector-ref v 0))) dvs))
  (define dys (map (λ (v) (exact->inexact (vector-ref v 1))) dvs))
  (define angles (map (λ (dx dy) (flatan2 (- dy) dx)) dxs dys))
  
  (define mags
    (match arrow-length
      [(? real?)  (build-list (length dxs) (λ _ arrow-length))]
      ['normalized
       (define box-x-size
         (send area plot->dc/x-size (/ (- x-max x-min) (sub1 samples))))
       (define box-y-size
         (send area plot->dc/y-size (/ (- y-max y-min) (sub1 samples))))
       (define box-size (min box-x-size box-y-size))
       (build-list (length dxs) (λ _ box-size))]
      ['real
       (map (λ (dx dy)
              (sqrt (+ (sqr (send area plot->dc/x-size dx))
                       (sqr (send area plot->dc/y-size dy)))))
            dxs dys)]
      ['scaled
       (define dx-max (apply max (map abs dxs)))
       (define dy-max (apply max (map abs dys)))
       (define box-x-size (/ (- x-max x-min) (sub1 samples)))
       (define box-y-size (/ (- y-max y-min) (sub1 samples)))
       (define scale (min (/ box-x-size dx-max) (/ box-y-size dy-max)))
       (map (λ (dx dy)
              (sqrt (+ (sqr (* scale (send area plot->dc/x-size dx)))
                       (sqr (* scale (send area plot->dc/y-size dy))))))
            dxs dys)]))
  
  (send area set-alpha alpha)
  (send area set-pen color line-width line-style)
  (for ([x      (in-list xs)]
        [y      (in-list ys)]
        [angle  (in-list angles)]
        [mag    (in-list mags)])
    (send area add-arrow-glyph (vector x y) (/ mag 2) angle)))

(defproc (vector-field
          [f (real? real? . -> . (vector/c real? real?))]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:color color plot-color/c (vector-field-color)]
          [#:line-width line-width nonnegative-real/c (vector-field-line-width)]
          [#:line-style line-style pen-style/c (vector-field-line-style)]
          [#:arrow-length arrow-length (one-of/c 'scaled 'normalized 'real)
                          (vector-field-arrow-length)]
          [#:alpha alpha (real-in 0 1) (vector-field-alpha)]
          ) renderer2d?
  (make-renderer2d (add-vectors f color line-width line-style
                                arrow-length alpha)
                   (default-range->ticks (plot2d-tick-skip))
                   (default-range->ticks (plot2d-tick-skip))
                   values
                   x-min x-max y-min y-max))

;; =============================================================================
;; Error bars

(define ((add-error-bars bars color line-width line-style width alpha) area)
  (define half (* 1/2 (send area dc->plot/x-size width)))
  
  (send area set-alpha alpha)
  (send area set-pen color line-width line-style)
  (for ([bar  (in-list bars)])
    (match-define (vector x y h) bar)
    (send area add-line (vector x (- y h)) (vector x (+ y h)))
    (send area add-line
          (vector (- x half) (- y h))
          (vector (+ x half) (- y h)))
    (send area add-line
          (vector (- x half) (+ y h))
          (vector (+ x half) (+ y h)))))

(define ((error-bars-bounds-function bars) x-min x-max y-min y-max)
  (define xs (delay (map (λ (v) (vector-ref v 0)) bars)))
  (define ys (delay (map (λ (v) (vector-ref v 1)) bars)))
  (define hs (delay (map (λ (v) (vector-ref v 2)) bars)))
  (let ([x-min  (if x-min x-min (apply regular-min (force xs)))]
        [x-max  (if x-max x-max (apply regular-max (force xs)))]
        [y-min  (if y-min y-min (apply regular-min
                                       (map - (force ys) (force hs))))]
        [y-max  (if y-max y-max (apply regular-max
                                       (map + (force ys) (force hs))))])
    (values x-min x-max y-min y-max)))

(defproc (error-bars
          [bars (listof (vector/c real? real? real?))]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:color color plot-color/c (error-bar-color)]
          [#:line-width line-width nonnegative-real/c (error-bar-line-width)]
          [#:line-style line-style nonnegative-real/c (error-bar-line-style)]
          [#:width width nonnegative-real/c (error-bar-width)]
          [#:alpha alpha (real-in 0 1) (error-bar-alpha)]
          ) renderer2d?
  (make-renderer2d (add-error-bars bars color line-width line-style
                                   width alpha)
                   (default-range->ticks (plot2d-tick-skip))
                   (default-range->ticks (plot2d-tick-skip))
                   (error-bars-bounds-function bars)
                   x-min x-max y-min y-max))
