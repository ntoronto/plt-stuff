#lang racket/base

(require racket/gui
         "../common/utils.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define line-samples (make-parameter 500))
(define line-color (make-parameter "red"))
(define line-width (make-parameter 1))
(define line-style (make-parameter 'solid))

(define (lines xs ys
               #:color [color (line-color)]
               #:width [width (line-width)]
               #:style [style (line-style)]
               #:x-min [x-min #f] #:x-max [x-max #f]
               #:y-min [y-min #f] #:y-max [y-max #f])
  (let ([x-min  (if x-min x-min (apply min xs))]
        [x-max  (if x-max x-max (apply max xs))]
        [y-min  (if y-min y-min (apply min ys))]
        [y-max  (if y-max y-max (apply max ys))])
    (renderer2d (λ (area)
                  (define pen (send area get-pen))
                  (send area set-pen color width style)
                  (send area draw-lines (map vector xs ys))
                  (send area set-pen pen))
                x-min x-max y-min y-max)))

(define (function f x-min x-max
                  #:color [color (line-color)]
                  #:width [width (line-width)]
                  #:style [style (line-style)]
                  #:y-min [y-min #f] #:y-max [y-max #f])
  (define xs (real-seq x-min x-max (line-samples)))
  (lines xs (map f xs)
         #:color color #:width width #:style style
         #:y-min y-min #:y-max y-max))

(define (parametric fx fy t-min t-max
                    #:color [color (line-color)]
                    #:width [width (line-width)]
                    #:style [style (line-style)]
                    #:x-min [x-min #f] #:x-max [x-max #f]
                    #:y-min [y-min #f] #:y-max [y-max #f])
  (define ts (real-seq t-min t-max (line-samples)))
  (lines (map fx ts) (map fy ts)
         #:color color #:width width #:style style
         #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max))
