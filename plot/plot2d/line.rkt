#lang racket/base

(require racket/contract racket/class racket/match racket/math racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "renderer.rkt"
         "ticks.rkt"
         "sample.rkt")

(provide (all-defined-out))

(defparam line-samples (integer>=/c 2) 500)
(defparam line-color plot-color/c 1)
(defparam line-width (real>=/c 0) 1)
(defparam line-style line-style/c 'solid)
(defparam line-alpha (real-in 0 1) 1)

;; ===================================================================================================
;; Lines, parametric, polar

(define ((lines-render-proc vs color width style alpha label) area)
  (send area set-alpha alpha)
  (send area set-pen color width style)
  (send area put-lines vs)
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(defproc (lines [vs  (listof (vector/c real? real?))]
                [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                [#:color color plot-color/c (line-color)]
                [#:width width (real>=/c 0) (line-width)]
                [#:style style line-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? #f) #f]
                ) renderer2d?
  (define rvs (filter vall-regular? vs))
  (cond [(empty? rvs)  null-renderer2d]
        [else
         (match-define (list (vector rxs rys) ...) rvs)
         (let ([x-min  (if x-min x-min (apply min* rxs))]
               [x-max  (if x-max x-max (apply max* rxs))]
               [y-min  (if y-min y-min (apply min* rys))]
               [y-max  (if y-max y-max (apply max* rys))])
           (make-renderer2d (lines-render-proc vs color width style alpha label)
                            default-2d-ticks-fun
                            null-2d-bounds-fun
                            x-min x-max y-min y-max))]))

(defproc (parametric [f (real? . -> . (vector/c real? real?))]
                     [t-min real?] [t-max real?]
                     [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                     [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                     [#:samples samples (integer>=/c 2) (line-samples)]
                     [#:color color plot-color/c (line-color)]
                     [#:width width (real>=/c 0) (line-width)]
                     [#:style style line-style/c (line-style)]
                     [#:alpha alpha (real-in 0 1) (line-alpha)]
                     [#:label label (or/c string? #f) #f]
                     ) renderer2d?
  (lines (sample-parametric f t-min t-max samples)
         #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
         #:color color #:width width #:style style #:alpha alpha
         #:label label))

(defproc (polar [f (real? . -> . real?)]
                [θ-min real? 0] [θ-max real? 2pi]
                [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                [#:samples samples (integer>=/c 2) (line-samples)]
                [#:color color plot-color/c (line-color)]
                [#:width width (real>=/c 0) (line-width)]
                [#:style style line-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? #f) #f]
                ) renderer2d?
  (lines (sample-2d-polar f θ-min θ-max samples)
         #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
         #:color color #:width width #:style style #:alpha alpha
         #:label label))

;; ===================================================================================================
;; Function

(define ((function-render-proc f samples color width style alpha label) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (match-define (list xs ys) (f x-min x-max samples))
  
  (send area set-alpha alpha)
  (send area set-pen color width style)
  (send area put-lines (map vector xs ys))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(define ((function-bounds-fun f samples) x-min x-max y-min y-max)
  (cond [(and x-min x-max)  (match-define (list xs ys) (f x-min x-max samples))
                            (define rys (filter regular? ys))
                            (cond [(empty? rys)  (values x-min x-max y-min y-max)]
                                  [else  (values x-min x-max
                                                 (if y-min y-min (apply min* rys))
                                                 (if y-max y-max (apply max* rys)))])]
        [else  (values x-min x-max y-min y-max)]))

(defproc (function [f (real? . -> . real?)]
                   [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
                   [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                   [#:samples samples (integer>=/c 2) (line-samples)]
                   [#:color color plot-color/c (line-color)]
                   [#:width width (real>=/c 0) (line-width)]
                   [#:style style line-style/c (line-style)]
                   [#:alpha alpha (real-in 0 1) (line-alpha)]
                   [#:label label (or/c string? #f) #f]
                   ) renderer2d?
  (define g (function->sampler f))
  (make-renderer2d (function-render-proc g samples color width style alpha label)
                   default-2d-ticks-fun
                   (function-bounds-fun g samples)
                   x-min x-max y-min y-max))

;; ===================================================================================================
;; Inverse function

(define ((inverse-render-proc f samples color width style alpha label) area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (match-define (list ys xs) (f y-min y-max samples))
  
  (send area set-alpha alpha)
  (send area set-pen color width style)
  (send area put-lines (map vector xs ys))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(define ((inverse-bounds-fun f samples) x-min x-max y-min y-max)
  (cond [(and y-min y-max)  (match-define (list ys xs) (f y-min y-max samples))
                            (define rxs (filter regular? xs))
                            (cond [(empty? rxs)  (values x-min x-max y-min y-max)]
                                  [else  (values (if x-min x-min (apply min* rxs))
                                                 (if x-max x-max (apply max* rxs))
                                                 y-min y-max)])]
        [else  (values x-min x-max y-min y-max)]))

(defproc (inverse [f (real? . -> . real?)]
                  [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
                  [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                  [#:samples samples (integer>=/c 2) (line-samples)]
                  [#:color color plot-color/c (line-color)]
                  [#:width width (real>=/c 0) (line-width)]
                  [#:style style line-style/c (line-style)]
                  [#:alpha alpha (real-in 0 1) (line-alpha)]
                  [#:label label (or/c string? #f) #f]
                  ) renderer2d?
  (define g (inverse->sampler f))
  (make-renderer2d (inverse-render-proc g samples color width style alpha label)
                   default-2d-ticks-fun
                   (inverse-bounds-fun g samples)
                   x-min x-max y-min y-max))
