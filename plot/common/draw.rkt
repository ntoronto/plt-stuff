#lang racket/base

(require racket/draw racket/class racket/vector racket/match racket/list
         racket/contract
         "math.rkt"
         "contract.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Drawing text rotated around an anchor point

(define (draw-text/anchor dc str x y [anchor 'top-left] [combine? #f] [offset 0] [angle 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f combine? offset))
  (define dx (case anchor
               [(top-left left bottom-left)     0]
               [(top center bottom)             (* 1/2 width)]
               [(top-right right bottom-right)  width]
               [else  (raise-type-error 'draw-text/anchor "anchor/c" anchor)]))
  (define dy (case anchor
               [(top-left top top-right)           0]
               [(left center right)                (* 1/2 height)]
               [(bottom-left bottom bottom-right)  height]))
  (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
  (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
  
  (send dc draw-text str (- x rdx) (- y rdy) combine? offset angle))

(define (get-text-corners/anchor dc str x y [anchor 'top-left] [combine? #f] [offset 0] [angle 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f combine? offset))
  (define dxs (case anchor
                [(top-left left bottom-left)     (list 0 width)]
                [(top center bottom)             (list (* -1/2 width) (* 1/2 width))]
                [(top-right right bottom-right)  (list (- width) 0)]
                [else  (raise-type-error 'get-text-corners/anchor "anchor/c" anchor)]))
  (define dys (case anchor
                [(top-left top top-right)           (list 0 height)]
                [(left center right)                (list (* -1/2 height) (* 1/2 width))]
                [(bottom-left bottom bottom-right)  (list (- height) 0)]))
  
  (for*/list ([dx  (in-list dxs)] [dy  (in-list dys)])
    (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
    (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
    (vector (+ x rdx) (+ y rdy))))

;; =============================================================================
;; Draw paramter normalization

(define (real->font-size size)
  (define i (inexact->exact (round size)))
  (min (max i 1) 255))

(define (real->color-byte f)
  (define i (inexact->exact (floor f)))
  (min (max i 0) 255))

(define (color%? c) (is-a? c color%))

(define (->color c)
  (match c
    [(? color%?)  (list (send c red) (send c green) (send c blue))]
    [(? string?)  (define color (send the-color-database find-color c))
                  (when (not color) (error 'decode-color "unknown color name ~e" c))
                  (->color color)]
    [(list (? real?) (? real?) (? real?))  c]
    [_  (error 'decode-color "unable to convert to color triple: ~e" c)]))

(define (color->color% c)
  (match-define (list r g b) c)
  (make-object color% (real->color-byte r) (real->color-byte g) (real->color-byte b)))

(define line-colors
  '#((0 0 0)          ; black
     (128 0 0)        ; red
     (0 96 0)         ; green
     (0 0 160)        ; blue
     (192 96 0)       ; yellow
     (0 112 128)      ; cyan
     (160 32 240)     ; magenta
     (160 160 160)))  ; gray

(define (->color/line color)
  (cond [(integer? color)  (vector-ref line-colors (remainder (abs color) 8))]
        [else              (->color color)]))

(define fill-colors
  '#((255 255 255)    ; white
     (255 192 192)    ; red
     (192 255 192)    ; green
     (212 224 240)    ; blue
     (255 248 192)    ; yellow
     (192 240 255)    ; cyan
     (240 224 255)    ; magenta
     (212 212 212)))  ; gray

(define (->color/fill color)
  (cond [(integer? color)  (vector-ref fill-colors (remainder (abs color) 8))]
        [else              (->color color)]))

(define (->style/line style)
  (cond [(integer? style)  (case (remainder (abs style) 5)
                             [(0)  'solid]
                             [(1)  'dot]
                             [(2)  'long-dash]
                             [(3)  'short-dash]
                             [(4)  'dot-dash])]
        [(symbol? style)   style]
        [else  (raise-type-error '->line-style "symbol or integer" style)]))

(define (->style/fill style)
  (cond [(integer? style)  (case (remainder (abs style) 7)
                             [(0)  'solid]
                             [(1)  'bdiagonal-hatch]
                             [(2)  'fdiagonal-hatch]
                             [(3)  'crossdiag-hatch]
                             [(4)  'horizontal-hatch]
                             [(5)  'vertical-hatch]
                             [(6)  'cross-hatch])]
        [(symbol? style)   style]
        [else  (raise-type-error '->fill-style "symbol or integer" style)]))

;; ===================================================================================================
;; Color functions

(define (rgb->hsv rgb)
  (match-define (list r g b) (map (λ (x) (/ x 255)) rgb))
  (define mx (max r g b))
  (define mn (min r g b))
  (define c (- mx mn))
  (define h (* 60 (cond [(zero? c)  0]
                        [(= mx r)   (/ (- g b) c)]
                        [(= mx g)   (+ (/ (- b r) c) 2)]
                        [(= mx b)   (+ (/ (- r g) c) 4)])))
  (list (if (h . < . 0) (+ h 360) h)
        (if (zero? mx) 0 (/ c mx))
        mx))

(define (hsv->rgb hsv)
  (match-define (list h s v) hsv)
  (define c (* v s))
  (let ([h  (/ (real-modulo h 360) 60)])
    (define x (* c (- 1 (abs (- (real-modulo h 2) 1)))))
    (define-values (r g b)
      (cond [(and (0 . <= . h) (h . < . 1))  (values c x 0)]
            [(and (1 . <= . h) (h . < . 2))  (values x c 0)]
            [(and (2 . <= . h) (h . < . 3))  (values 0 c x)]
            [(and (3 . <= . h) (h . < . 4))  (values 0 x c)]
            [(and (4 . <= . h) (h . < . 5))  (values x 0 c)]
            [(and (5 . <= . h) (h . < . 6))  (values c 0 x)]))
    (define m (- v c))
    (list (* 255 (+ r m))
          (* 255 (+ g m))
          (* 255 (+ b m)))))

(define (color-seq c1 c2 num #:start? [start? #t] #:end? [end? #t])
  (match-define (list r1 g1 b1) (->color c1))
  (match-define (list r2 g2 b2) (->color c2))
  (define rs (linear-seq r1 r2 num #:start? start? #:end? end?))
  (define gs (linear-seq g1 g2 num #:start? start? #:end? end?))
  (define bs (linear-seq b1 b2 num #:start? start? #:end? end?))
  (map list rs gs bs))

(define (color-seq* colors num #:start? [start? #t] #:end? [end? #t])
  (when (empty? colors) (raise-type-error 'color-seq* "nonempty (listof plot-color/c)" colors))
  (match-define (list (list rs gs bs) ...) (map ->color colors))
  (let ([rs  (linear-seq* rs num #:start? start? #:end? end?)]
        [gs  (linear-seq* gs num #:start? start? #:end? end?)]
        [bs  (linear-seq* bs num #:start? start? #:end? end?)])
    (map list rs gs bs)))

(define (mix-colors c1 c2 a)
  (map (λ (x1 x2) (alpha-blend x1 x2 a))
       (->color c1) (->color c2)))

(define (alpha-expt a n)
  (- 1 (expt (- 1 a) n)))

(define (apply-colors colors xs)
  (cond [(procedure? colors)  (colors xs)]
        [else                 colors]))

(define apply-fill-styles apply-colors)
(define apply-line-widths apply-colors)
(define apply-line-styles apply-colors)
(define apply-alphas apply-colors)
