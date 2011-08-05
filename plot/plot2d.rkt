#lang racket/base

#|
TODO:

Ongoing: give the library the interface *I* think it should have

error-bars

Contracts!

Tests!

Compatibility layer

Render list / axes+ticks always in front

Fill lowest contour level using clear

'points' symbols:
  integers -1..127
  5star
  6star
  fullsquare
  full5star
|#

(require "plot2d/area.rkt"
         "plot2d/renderer.rkt"
         "plot2d/plot.rkt"
         "plot2d/lines.rkt"
         "plot2d/points.rkt"
         "plot2d/contour.rkt")

(provide (all-from-out
          "plot2d/area.rkt"
          "plot2d/renderer.rkt"
          "plot2d/plot.rkt"
          "plot2d/lines.rkt"
          "plot2d/points.rkt"
          "plot2d/contour.rkt"))

(require racket/math)

(define xs (build-list 10 (λ _ (random))))
(define ys (build-list 10 (λ _ (random))))
(time
 (plot2d (points (map vector xs ys)
                 #:color "black"
                 #:sym 'leftarrow #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1)))

(time
 (plot2d (function sin -1/2 1)
         (parametric cos sin -2 1
                     #:color "blue")))

(time
 (plot2d (vector-field values #:style 'scaled)
         #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1))

; tests contour 7-sided and 8-sided saddle facets
; contour shading should line up with contour lines, no matter how weird
(parameterize ([contour-samples  10])
  (define (f x y) (sqr (sin (- x y))))
  (time (plot2d (shade f) (contour f))))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (+ (norm -1.5 -1.5 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))))

(time (plot2d (shade f1) (contour f1)))

(time
 (parameterize ([title-string  "Survival Rate of Torsion Widgets"]
                [x-axis-string "Torsion"]
                [y-axis-string "Widgetyness"])
   (plot2d/file "contour-test.png" 'png (contour f1))))
