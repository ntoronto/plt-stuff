#lang racket/base

(require racket/gui
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/points.rkt"
         "../common/ticks.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (except-out (all-defined-out)
                     add-histogram histogram-ticks))

(define histogram-bar-color (make-parameter "white"))
(define histogram-bar-style (make-parameter 'solid))
(define histogram-line-color (make-parameter "black"))
(define histogram-line-width (make-parameter 2/3))
(define histogram-line-style (make-parameter 'solid))
(define histogram-alpha (make-parameter 1.0))

(define ((add-histogram cats ys x-min x-max bar-color bar-style
                        line-color line-width line-style alpha) area)
  (define n (length ys))
  (define step (/ (- x-max x-min) n))
  (send area set-pen line-color line-width line-style)
  (send area set-brush bar-color bar-style)
  (send area set-alpha alpha)
  (for ([y  (in-list ys)] [i  (in-naturals)])
    (define x (+ x-min (* i step)))
    (send area draw-rectangle (vector x 0) (vector (+ x step) y))))

(define ((histogram-ticks cats x-min x-max) . _)
  (define vec (list->vector cats))
  (define n (vector-length vec))
  (define step (/ (- x-max x-min) n))
  (for/list ([cat  (in-list cats)] [i  (in-naturals)])
    (tick (+ x-min (* i step) (* 1/2 step))
          (any->tick-label cat)
          #t)))

(define (histogram cat-vals [y-min #f] [y-max #f]
                   #:x-min [x-min 0] #:x-max [x-max #f]
                   #:bar-color [bar-color (histogram-bar-color)]
                   #:bar-style [bar-style (histogram-bar-style)]
                   #:line-color [line-color (histogram-line-color)]
                   #:line-width [line-width (histogram-line-width)]
                   #:line-style [line-style (histogram-line-style)]
                   #:alpha [alpha (histogram-alpha)])
  (match-define (list (vector cats ys) ...) cat-vals)
  (let ([x-max  (if x-max x-max (+ x-min (length ys)))]
        [y-min  (if y-min y-min (apply regular-min ys))]
        [y-max  (if y-max y-max (apply regular-max ys))])
    (make-renderer2d (add-histogram cats ys x-min x-max bar-color bar-style
                                    line-color line-width line-style alpha)
                     (histogram-ticks cats x-min x-max)
                     (default-range->ticks (plot2d-tick-skip))
                     values
                     x-min x-max y-min y-max)))
