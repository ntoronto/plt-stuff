#lang racket/base

#|
TODO

Ticks

Tick labels (always in front)

Axis labels (always in front)

Title

Autosize plot area

Projected "shadows" on rear axial planes?

Handle error cases
 - Normalizing zero vectors
 - Empty lists
 - +nan.0
 - +inf.0 / -inf.0

Contracts!

Test cases!

Compatibility layer
|#


(require "plot3d/area.rkt"
         "plot3d/renderer.rkt"
         "plot3d/plot.rkt"
         "plot3d/surface.rkt"
         ;"plot3d/lines.rkt"
         ;"plot3d/points.rkt"
         )

(provide (all-from-out
          "plot3d/area.rkt"
          "plot3d/renderer.rkt"
          "plot3d/plot.rkt"
          "plot3d/surface.rkt"
          ;"plot3d/lines.rkt"
          ;"plot3d/points.rkt"
          ))

;; =============================================================================

(require racket/math racket/list)

(plot3d-width 320)
(plot3d-height 280)

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (- (sqr x) (sqr y)))

(define (f2 x y)
  (- (sqrt (+ (abs y) (abs x)))))

(define (f3 x y)
  (define d (* 2 pi (+ (abs x) (abs y))))
  (+ (* 1/8 (cos d)) (- (sqr x) (sqr y))))

(define (f4 x y)
  (imag-part (log (make-rectangular x y))))

(define (f5 x y)
  (+ (norm -1.5 -1.5 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))
     -0.1))

(time
 (plot3d (surface3d f5 -4 4 -4 4 #:color '(128 160 255))))

(time
 (plot3d (surface3d f5 -4 4 -4 4 #:color '(224 128 255))
         (contour3d f5 -4 4 -4 4 #:line-width 2)))

(time
 (plot3d (shade3d f5 -4 4 -4 4)
         (contour3d f5 -4 4 -4 4 #:line-color '(128 0 128))))

;; error to fix: this one has a zero surface normal somewhere
(time
 (plot3d (shade3d f1 -4 4 -4 4)
         (contour3d f1 -4 4 -4 4)))
