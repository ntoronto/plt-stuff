#lang racket/base

(require "../common/vector.rkt")

(provide (all-defined-out))

(struct shape (alpha center) #:transparent)

(struct polygon shape
        (vs pen-color pen-width pen-style brush-color brush-style)
        #:transparent)
(struct line shape (v1 v2 pen-color pen-width pen-style) #:transparent)
(struct text shape (anchor angle str font-size font-family color) #:transparent)
(struct point shape (pen-color pen-width pen-style) #:transparent)
(struct circle-glyph shape
        (r pen-color pen-width pen-style brush-color brush-style)
        #:transparent)
(struct polygon-glyph shape
        (r sides start-angle
           pen-color pen-width pen-style brush-color brush-style)
        #:transparent)
(struct flare-glyph shape
        (r sticks start-angle pen-color pen-width pen-style)
        #:transparent)
(struct arrow-glyph shape
        (r angle pen-color pen-width pen-style)
        #:transparent)

(define (shape-normal s)
  (cond [(polygon? s)  (surface-normal (polygon-vs s))]
        [else          default-normal]))

(define (shape-coords s)
  (cond [(polygon? s)  (polygon-vs s)]
        [(line? s)     (list (line-v1 s) (line-v2 s))]
        [(shape? s)    (list (shape-center s))]))

(define (draw-before? s1 s2)
  (define c1 (vector-ref (shape-center s1) 1))
  (define c2 (vector-ref (shape-center s2) 1))
  (or (c1 . > . c2)
      (and (polygon? s1) (not (polygon? s2))
           (c1 . = . c2))))

(define (depth-sort shapes)
  (sort shapes draw-before?))
