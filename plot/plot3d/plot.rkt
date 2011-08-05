#lang racket/base

(require racket/gui racket/match
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define plot3d-width (make-parameter 400))
(define plot3d-height (make-parameter 350))

(define plot3d-x-min-margin (make-parameter 0))
(define plot3d-x-max-margin (make-parameter 0))
(define plot3d-y-min-margin (make-parameter 0))
(define plot3d-y-max-margin (make-parameter 0))
(define plot3d-z-min-margin (make-parameter 0))
(define plot3d-z-max-margin (make-parameter 0))

(define plot3d-x-min (make-parameter -5))
(define plot3d-x-max (make-parameter 5))
(define plot3d-y-min (make-parameter -5))
(define plot3d-y-max (make-parameter 5))
(define plot3d-z-min (make-parameter -5))
(define plot3d-z-max (make-parameter 5))

(define plot3d-angle (make-parameter 30))
(define plot3d-altitude (make-parameter 20))

;; plot3d : renderer ... -> 3d-plot-snip%
(define (plot3d #:width [width (plot3d-width)]
                #:height [height (plot3d-height)]
                #:angle [angle (plot3d-angle)]
                #:altitude [altitude (plot3d-altitude)]
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f]
                #:z-min [z-min #f] #:z-max [z-max #f]
                . renderers)
  (match-define (renderer3d f rx-min rx-max ry-min ry-max rz-min rz-max)
    (apply mix3d renderers))
  (let ([x-min  (if x-min x-min (if rx-min rx-min (plot3d-x-min)))]
        [x-max  (if x-max x-max (if rx-max rx-max (plot3d-x-max)))]
        [y-min  (if y-min y-min (if ry-min ry-min (plot3d-y-min)))]
        [y-max  (if y-max y-max (if ry-max ry-max (plot3d-y-max)))]
        [z-min  (if z-min z-min (if rz-min rz-min (plot3d-z-min)))]
        [z-max  (if z-max z-max (if rz-max rz-max (plot3d-z-max)))])
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define z-size (- z-max z-min))
    (define snip
      (make-object 3d-plot-snip% width height
        (- x-min (* x-size (plot3d-x-min-margin)))
        (+ x-max (* x-size (plot3d-x-max-margin)))
        (- y-min (* y-size (plot3d-y-min-margin)))
        (+ y-max (* y-size (plot3d-y-max-margin)))
        (- z-min (* z-size (plot3d-z-min-margin)))
        (+ z-max (* z-size (plot3d-z-max-margin)))
        (* (/ pi 180) angle) (* (/ pi 180) altitude)))
    (f (send snip get-area))
    (send (send snip get-area) end-doc)
    snip))
