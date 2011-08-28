#lang racket/base

(require racket/contract racket/promise racket/class racket/match
         "../common/math.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "../common/vector.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(defparam line-samples samples positive-integer/c 500)
(defparam line-color color plot-color/c "darkred")
(defparam line-width width nonnegative-real/c 1)
(defparam line-style style pen-style/c 'solid)
(defparam line-alpha alpha (real-in 0 1) 1)

(define (memoize f)
  (define memo (make-hasheqv))
  (λ (x) (hash-ref! memo x (λ () (f x)))))

(defproc (lines
          [vs  (listof (vector/c real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:color color plot-color/c (line-color)]
          [#:width width nonnegative-real/c (line-width)]
          [#:style style pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]) renderer2d?
  (define xs (delay (map (λ (xy) (vector-ref xy 0)) vs)))
  (define ys (delay (map (λ (xy) (vector-ref xy 1)) vs)))
  (let ([x-min  (if x-min x-min (apply regular-min (force xs)))]
        [x-max  (if x-max x-max (apply regular-max (force xs)))]
        [y-min  (if y-min y-min (apply regular-min (force ys)))]
        [y-max  (if y-max y-max (apply regular-max (force ys)))])
    (make-renderer2d (λ (area)
                       (send area set-alpha alpha)
                       (send area set-pen color width style)
                       (send area add-lines vs))
                     (default-range->ticks (plot2d-tick-skip))
                     (default-range->ticks (plot2d-tick-skip))
                     values
                     x-min x-max y-min y-max)))

(defproc (parametric
          [fxy (or/c (real? . -> . (vector/c real? real?))
                     (vector/c (real? . -> . real?) (real? . -> . real?)))]
          [t-min real?] [t-max real?]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:samples samples positive-integer/c (line-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width nonnegative-real/c (line-width)]
          [#:style style pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]) renderer2d?
  (define ts (real-seq t-min t-max samples))
  (lines (match fxy
           [(vector fx fy)  (map vector (map fx ts) (map fy ts))]
           [(? procedure?)  (map fxy ts)])
         #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
         #:color color #:width width #:style style #:alpha alpha))

(define ((add-function f samples color width style alpha) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define xs (real-seq x-min x-max samples))
  (define ys (map f xs))
  (send area set-alpha alpha)
  (send area set-pen color width style)
  (send area add-lines (map vector xs ys)))

(define ((function-bounds-function f samples) x-min x-max y-min y-max)
  (define ys (delay (map f (real-seq x-min x-max samples))))
  (let ([y-min  (if (and (not y-min) x-min x-max)
                    (apply regular-min (force ys))
                    y-min)]
        [y-max  (if (and (not y-max) x-min x-max)
                    (apply regular-max (force ys))
                    y-max)])
    (values x-min x-max y-min y-max)))

(defproc (function
          [f (real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:samples samples positive-integer/c (line-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width nonnegative-real/c (line-width)]
          [#:style style pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]) renderer2d?
  (define g (memoize f))
  (make-renderer2d (add-function g samples color width style alpha)
                   (default-range->ticks (plot2d-tick-skip))
                   (default-range->ticks (plot2d-tick-skip))
                   (function-bounds-function g samples)
                   x-min x-max y-min y-max))

(define ((add-inverse f samples color width style alpha) area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define ys (real-seq y-min y-max samples))
  (define xs (map f ys))
  (send area set-alpha alpha)
  (send area set-pen color width style)
  (send area add-lines (map vector xs ys)))

(define ((inverse-bounds-function f samples) x-min x-max y-min y-max)
  (let* ([xs     (delay (map f (real-seq y-min y-max samples)))]
         [x-min  (if (and (not x-min) y-min y-max)
                     (apply regular-min (force xs))
                     x-min)]
         [x-max  (if (and (not x-max) y-min y-max)
                     (apply regular-max (force xs))
                     x-max)])
    (values x-min x-max y-min y-max)))

(defproc (inverse
          [f (real? . -> . real?)]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:samples samples positive-integer/c (line-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width nonnegative-real/c (line-width)]
          [#:style style pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]) renderer2d?
  (define g (memoize f))
  (make-renderer2d (add-inverse g samples color width style alpha)
                   (default-range->ticks (plot2d-tick-skip))
                   (default-range->ticks (plot2d-tick-skip))
                   (inverse-bounds-function g samples)
                   x-min x-max y-min y-max))
