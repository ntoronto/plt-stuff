#lang racket/base

(require racket/gui racket/match
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define plot2d-width (make-parameter 400))
(define plot2d-height (make-parameter 350))

(define plot2d-x-min-margin (make-parameter 0))
(define plot2d-x-max-margin (make-parameter 0))
(define plot2d-y-min-margin (make-parameter 0))
(define plot2d-y-max-margin (make-parameter 0))

(define plot2d-x-min (make-parameter -5))
(define plot2d-x-max (make-parameter 5))
(define plot2d-y-min (make-parameter -5))
(define plot2d-y-max (make-parameter 5))

;; plot2d : renderer ... -> 2d-plot-snip%
(define (plot2d #:width [width (plot2d-width)]
                #:height [height (plot2d-height)]
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f]
                . renderers)
  (match-define (renderer2d f rx-min rx-max ry-min ry-max)
    (apply mix2d renderers))
  (let ([x-min  (if x-min x-min (if rx-min rx-min (plot2d-x-min)))]
        [x-max  (if x-max x-max (if rx-max rx-max (plot2d-x-max)))]
        [y-min  (if y-min y-min (if ry-min ry-min (plot2d-y-min)))]
        [y-max  (if y-max y-max (if ry-max ry-max (plot2d-y-max)))])
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define snip
      (make-object 2d-plot-snip% width height
        (- x-min (* x-size (plot2d-x-min-margin)))
        (+ x-max (* x-size (plot2d-x-max-margin)))
        (- y-min (* y-size (plot2d-y-min-margin)))
        (+ y-max (* y-size (plot2d-y-max-margin)))))
    (f (send snip get-area))
    snip))

(define (plot2d/file name kind #:quality [quality 75]
                     #:width [width (plot2d-width)]
                     #:height [height (plot2d-height)]
                     #:x-min [x-min #f] #:x-max [x-max #f]
                     #:y-min [y-min #f] #:y-max [y-max #f]
                     . renderers)
  (match-define (renderer2d f rx-min rx-max ry-min ry-max)
    (apply mix2d renderers))
  (let ([x-min  (if x-min x-min (if rx-min rx-min (plot2d-x-min)))]
        [x-max  (if x-max x-max (if rx-max rx-max (plot2d-x-max)))]
        [y-min  (if y-min y-min (if ry-min ry-min (plot2d-y-min)))]
        [y-max  (if y-max y-max (if ry-max ry-max (plot2d-y-max)))])
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define bm (make-bitmap width height))
    (define dc (make-object bitmap-dc% bm))
    (send dc set-background (bg-color))
    (send dc clear)
    (send dc set-font (make-object font% (font-size) 'roman))
    (send dc set-pen (fg-color) (pen-width) 'solid)
    (send dc set-smoothing 'smoothed)
    (define area (make-object 2d-plot-area% x-min x-max y-min y-max dc))
    (send area decorate-plot)
    (f area)
    (send area clip-to-whole)
    (send bm save-file name kind quality)))
