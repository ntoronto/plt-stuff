#lang slideshow

(require "../plot2d.rkt"
         "../plot3d.rkt")

(plot2d-font-size (* 2 (plot2d-font-size)))
(plot2d-pen-width (* 2 (plot2d-pen-width)))

(plot3d-font-size (* 2 (plot3d-font-size)))
(plot3d-pen-width (* 2 (plot3d-pen-width)))

(slide
 #:title "A 2D Parabola"
 (item "This is a 2D parabola:")
 (bitmap (plot2d->bitmap (function sqr -1 1)
                         #:width (current-para-width) #:height 500)))

(slide
 #:title "A 3D Parabola"
 (item "This is a 3D parabola:")
 (bitmap (plot3d->bitmap
          (list (shade3d (λ (x y) (+ (sqr x) (sqr y))) -2 2 -2 2)
                (contour3d (λ (x y) (+ (sqr x) (sqr y))) -2 2 -2 2))
          #:width (current-para-width) #:height 500)))
