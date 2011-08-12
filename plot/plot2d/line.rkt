#lang racket/base

(require racket/gui
         "../common/math.rkt"
         "../common/ticks.rkt"
         "area.rkt"
         "renderer.rkt")

(provide line-samples line-color line-width line-style line-alpha
         lines parametric function)

(define line-samples (make-parameter 500))
(define line-color (make-parameter '(255 0 0)))
(define line-width (make-parameter 1))
(define line-style (make-parameter 'solid))
(define line-alpha (make-parameter 1.0))

(define (lines vs [x-min #f] [x-max #f] [y-min #f] [y-max #f]
               #:color [color (line-color)]
               #:width [width (line-width)]
               #:style [style (line-style)]
               #:alpha [alpha (line-alpha)])
  (let* ([xs     (delay (map (λ (xy) (vector-ref xy 0)) vs))]
         [ys     (delay (map (λ (xy) (vector-ref xy 1)) vs))]
         [x-min  (if x-min x-min (reg-min* (force xs)))]
         [x-max  (if x-max x-max (reg-max* (force xs)))]
         [y-min  (if y-min y-min (reg-min* (force ys)))]
         [y-max  (if y-max y-max (reg-max* (force ys)))])
    (renderer2d (λ (area)
                  (send area set-alpha alpha)
                  (send area set-pen color width style)
                  (send area add-lines vs))
                (default-range->ticks (plot2d-tick-skip))
                (default-range->ticks (plot2d-tick-skip))
                x-min x-max y-min y-max)))

(define (parametric fxy t-min t-max [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                    #:color [color (line-color)]
                    #:width [width (line-width)]
                    #:style [style (line-style)]
                    #:alpha [alpha (line-alpha)])
  (define ts (real-seq t-min t-max (line-samples)))
  (lines (match fxy
           [(vector fx fy)  (map vector (map fx ts) (map fy ts))]
           [(? procedure?)  (map fxy ts)])
        x-min x-max y-min y-max
        #:color color #:width width #:style style #:alpha alpha))

(define ((add-function f color width style alpha) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define xs (real-seq x-min x-max (line-samples)))
  (define ys (map f xs))
  (send area set-alpha alpha)
  (send area set-pen color width style)
  (send area add-lines (map vector xs ys)))

(define (memoize f)
  (define memo (make-hasheqv))
  (λ (x) (hash-ref! memo x (λ () (f x)))))

(define (function f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                  #:color [color (line-color)]
                  #:width [width (line-width)]
                  #:style [style (line-style)]
                  #:alpha [alpha (line-alpha)])
  (let* ([f      (memoize f)]
         [ys     (delay (map f (real-seq x-min x-max (line-samples))))]
         [y-min  (if (and (not y-min) x-min x-max)
                     (reg-min* (force ys))
                     y-min)]
         [y-max  (if (and (not y-max) x-min x-max)
                     (reg-max* (force ys))
                     y-max)])
    (renderer2d (add-function f color width style alpha)
                (default-range->ticks (plot2d-tick-skip))
                (default-range->ticks (plot2d-tick-skip))
                x-min x-max y-min y-max)))
