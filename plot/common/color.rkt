#lang racket/base

(require racket/match racket/gui
         "math.rkt")

(provide (all-defined-out))

(define (real->color-byte f)
  (let ([i  (inexact->exact (floor f))])
    (if (i . < . 0) 0 (if (i . > . 255) 255 i))))

(define (->color c)
  (match c
    [(? (λ (c) (is-a? c color%)))
     (list (send c red) (send c green) (send c blue))]
    [(? symbol?)
     (->color (symbol->string c))]
    [(? string?)
     (define color (send the-color-database find-color c))
     (when (not color)
       (error 'decode-color "unknown color name ~e" c))
     (->color color)]
    [(list (? real?) (? real?) (? real?))
     c]
    [_  (error 'decode-color "unable to convert to color triple: ~e" c)]))

(define (color->color% c)
  (match-define (list r g b) c)
  (make-object color%
    (real->color-byte r) (real->color-byte g) (real->color-byte b)))

(define (color-seq c1 c2 num)
  (match-define (list r1 g1 b1) (->color c1))
  (match-define (list r2 g2 b2) (->color c2))
  (define rs (map real->color-byte (real-seq r1 r2 num)))
  (define gs (map real->color-byte (real-seq g1 g2 num)))
  (define bs (map real->color-byte (real-seq b1 b2 num)))
  (map list rs gs bs))

(define (mix-colors c1 c2 a)
  (map (λ (x1 x2) (+ (* a x1) (* (- 1 a) x2))) c1 c2))

(define (color-seq* colors num)
  (cond
    [(empty? colors)  (build-list num (λ _ '(0 0 0)))]
    [(= num 1)        (first colors)]
    [(= num 0)        empty]
    [else
     (define cols (list->vector (map ->color colors)))
     (define len (vector-length cols))
     (define scale (/ (sub1 len) (sub1 num)))
     (build-list num (λ (n)
                       (define fidx (* n scale))
                       (define idx (floor fidx))
                       (cond [(= idx (sub1 len))  (vector-ref cols idx)]
                             [else  (mix-colors (vector-ref cols (add1 idx))
                                                (vector-ref cols idx)
                                                (- fidx idx))])))]))
