#lang racket/base

(require racket/match racket/contract racket/class racket/list
         "../common/math.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "renderer.rkt"
         "ticks.rkt")

(provide (all-defined-out))

(defparam histogram-color plot-color/c 3)
(defparam histogram-style fill-style/c 'solid)
(defparam histogram-line-color plot-color/c 3)
(defparam histogram-line-width (real>=/c 0) 1)
(defparam histogram-line-style line-style/c 'solid)
(defparam histogram-alpha (real-in 0 1) 1)

(define ((histogram-render-proc cats ys x-min x-max color style
                                line-color line-width line-style alpha
                                label)
         area)
  (define n (length ys))
  (define step (/ (- x-max x-min) n))
  (send area set-pen line-color line-width line-style)
  (send area set-brush color style)
  (send area set-alpha alpha)
  (for ([y  (in-list ys)] [i  (in-naturals)])
    (define x (+ x-min (* i step)))
    (send area put-rectangle (vector x 0) (vector (+ x step) y)))
  
  (cond [label  (rectangle-legend-entry
                 label color style line-color line-width line-style)]
        [else  empty]))

(define ((histogram-ticks-fun cats rx-min rx-max) _x-min _x-max y-min y-max)
  (define vec (list->vector cats))
  (define n (vector-length vec))
  (define step (/ (- rx-max rx-min) n))
  (define x-ticks
    (for/list ([cat  (in-list cats)] [i  (in-naturals)])
      (tick (+ rx-min (* i step) (* 1/2 step))
            (any->tick-label cat)
            #t)))
  (values x-ticks (default-2d-y-ticks-fun y-min y-max)))

(defproc (histogram
          [cat-vals (listof (vector/c any/c real?))]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:x-min x-min (or/c real? #f) 0] [#:x-max x-max (or/c real? #f) #f]
          [#:color color plot-color/c (histogram-color)]
          [#:style style fill-style/c (histogram-style)]
          [#:line-color line-color plot-color/c (histogram-line-color)]
          [#:line-width line-width (real>=/c 0) (histogram-line-width)]
          [#:line-style line-style line-style/c (histogram-line-style)]
          [#:alpha alpha (real-in 0 1) (histogram-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (match-define (list (vector cats ys) ...) cat-vals)
  (define rys (filter regular? ys))
  (cond
    [(empty? rys)  null-renderer2d]
    [else
     (let ([x-max  (if x-max x-max (+ x-min (length ys)))]
           [y-min  (if y-min y-min (apply min* rys))]
           [y-max  (if y-max y-max (apply max* rys))])
       (make-renderer2d
        (histogram-render-proc cats ys x-min x-max color style
                               line-color line-width line-style alpha label)
        (histogram-ticks-fun cats x-min x-max)
        null-2d-bounds-fun
        x-min x-max y-min y-max))]))
