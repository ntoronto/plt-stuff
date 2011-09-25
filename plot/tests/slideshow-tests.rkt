#lang slideshow

(require "../plot2d.rkt"
         "../plot3d.rkt"
         "../common.rkt")

(plot-font-size (* 2 (plot-font-size)))
(plot-pen-width (* 2 (plot-pen-width)))

(slide
 #:title "A 2D Parabola"
 (para "A 2D parabola:")
 (bitmap (plot2d (function sqr -1 1 #:label "y = x^2")
                 #:width (current-para-width) #:height 500)))

(slide
 #:title "A 3D Parabola"
 (para "A 3D parabola:")
 (bitmap (plot3d
          (list (surface3d (λ (x y) (+ (sqr x) (sqr y))) -2 2 -2 2
                                     #:label "z = x^2 + y^2" #:color 3)
                (contours3d (λ (x y) (+ (sqr x) (sqr y))) -2 2 -2 2))
          #:width (current-para-width) #:height 500
          #:legend-anchor 'top-left)))
