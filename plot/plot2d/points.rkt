#lang racket/base

(require racket/gui
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/points.rkt"
         "../common/ticks.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (except-out (all-defined-out)
                     add-vectors))

(define point-label (make-parameter 'circle))
(define point-color (make-parameter "black"))
(define point-size (make-parameter 6))
(define point-line-width (make-parameter 2/3))
(define point-alpha (make-parameter 1.0))

(define vector-field-samples (make-parameter 20))
(define vector-field-color (make-parameter "red"))
(define vector-field-line-width (make-parameter 2/3))
(define vector-field-arrow-length (make-parameter 'scaled))
(define vector-field-alpha (make-parameter 1.0))

(define (points vs [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                #:label [label (point-label)]
                #:color [color (point-color)]
                #:size [size (point-size)]
                #:line-width [line-width (point-line-width)]
                #:alpha [alpha (point-alpha)])
  (let* ([xs     (delay (map (λ (v) (vector-ref v 0)) vs))]
         [ys     (delay (map (λ (v) (vector-ref v 1)) vs))]
         [x-min  (if x-min x-min (reg-min* (force xs)))]
         [x-max  (if x-max x-max (reg-max* (force xs)))]
         [y-min  (if y-min y-min (reg-min* (force ys)))]
         [y-max  (if y-max y-max (reg-max* (force ys)))])
    (renderer2d (add-points vs label color size line-width alpha)
                (default-range->ticks (plot2d-tick-skip))
                (default-range->ticks (plot2d-tick-skip))
                x-min x-max y-min y-max)))

(define ((add-vectors f color line-width arrow-length alpha) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  
  (define samples (vector-field-samples))
  (define-values (vs dvs)
    (for*/lists (vs dvs) ([x   (in-list (real-seq x-min x-max samples))]
                          [y   (in-list (real-seq y-min y-max samples))]
                          [v   (in-value (vector x y))]
                          [dv  (in-value (f v))]
                          #:when (vreg? dv))
      (values v dv)))
  
  (define dxs (map (λ (v) (exact->inexact (vector-ref v 0))) dvs))
  (define dys (map (λ (v) (exact->inexact (vector-ref v 1))) dvs))
  (define angles (map (λ (dx dy) (flatan2 (- dy) dx)) dxs dys))
  
  (define mags
    (match arrow-length
      [(? real?)  (build-list (length dxs) (λ _ arrow-length))]
      ['normalized
       (define box-x-size
         (send area view->dc/x-size (/ (- x-max x-min) (sub1 samples))))
       (define box-y-size
         (send area view->dc/y-size (/ (- y-max y-min) (sub1 samples))))
       (define box-size (min box-x-size box-y-size))
       (build-list (length dxs) (λ _ box-size))]
      ['real
       (map (λ (dx dy)
              (sqrt (+ (sqr (send area view->dc/x-size dx))
                       (sqr (send area view->dc/y-size dy)))))
            dxs dys)]
      ['scaled
       (define dx-max (apply max (map abs dxs)))
       (define dy-max (apply max (map abs dys)))
       (define box-x-size (/ (- x-max x-min) (sub1 samples)))
       (define box-y-size (/ (- y-max y-min) (sub1 samples)))
       (define scale (min (/ box-x-size dx-max) (/ box-y-size dy-max)))
       (map (λ (dx dy)
              (sqrt (+ (sqr (* scale (send area view->dc/x-size dx)))
                       (sqr (* scale (send area view->dc/y-size dy))))))
            dxs dys)]))
  
  (send area set-alpha alpha)
  (send area set-pen color line-width 'solid)
  (for ([v      (in-list vs)]
        [angle  (in-list angles)]
        [mag    (in-list mags)])
    (send area add-arrow-glyph v (/ mag 2) angle)))

(define (vector-field f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                      #:color [color (vector-field-color)]
                      #:line-width [line-width (vector-field-line-width)]
                      #:arrow-length [arrow-length (vector-field-arrow-length)]
                      #:alpha [alpha (vector-field-alpha)])
  (renderer2d (add-vectors f color line-width arrow-length alpha)
              (default-range->ticks (plot2d-tick-skip))
              (default-range->ticks (plot2d-tick-skip))
              x-min x-max y-min y-max))
