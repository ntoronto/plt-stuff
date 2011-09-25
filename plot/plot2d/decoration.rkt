#lang racket/base

(require racket/contract racket/class racket/match racket/math racket/list
         "../common/ticks.rkt"
         "../common/math.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "../common/vector.rkt"
         "../common/area.rkt"
         "../common/sample.rkt"
         "renderer.rkt"
         "area.rkt"
         "line.rkt"
         "interval.rkt"
         "point.rkt"
         "contour.rkt"
         "sample.rkt")

(provide (all-defined-out))

(defparam x-axis-ticks? boolean? #f)
(defparam y-axis-ticks? boolean? #f)

(defparam polar-axes-number (integer>=/c 1) 12)
(defparam polar-axes-ticks? boolean? #t)

(defparam label-anchor anchor/c 'left)
(defparam label-angle real? 0)
(defparam label-alpha (real-in 0 1) 1)
(defparam label-point-size (real>=/c 0) 4)

;; ===================================================================================================
;; X and Y axes

(define ((x-axis-render-proc y ticks?) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define x-ticks (send area get-x-ticks))
  (define half (* 1/2 (plot2d-tick-size)))
  
  (send area set-minor-pen)
  (send area put-line (vector x-min y) (vector x-max y))
  
  (when ticks?
    (for ([t  (in-list x-ticks)])
      (match-define (tick x _ major?) t)
      (match-define (vector dc-x dc-y) (send area plot->dc (vector x y)))
      (if major? (send area set-major-pen) (send area set-minor-pen))
      (send area draw-line (vector dc-x (- dc-y half)) (vector dc-x (+ dc-y half)))))
  
  empty)

(define ((y-axis-render-proc x ticks?) area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define y-ticks (send area get-y-ticks))
  (define half (* 1/2 (plot2d-tick-size)))
  
  (send area set-minor-pen)
  (send area put-line (vector x y-min) (vector x y-max))
  
  (when ticks?
    (for ([t  (in-list y-ticks)])
      (match-define (tick y _ major?) t)
      (match-define (vector dc-x dc-y) (send area plot->dc (vector x y)))
      (if major? (send area set-major-pen) (send area set-minor-pen))
      (send area draw-line (vector (- dc-x half) dc-y) (vector (+ dc-x half) dc-y))))
  
  empty)

(defproc (x-axis [y real? 0] [#:ticks? ticks? boolean? (x-axis-ticks?)]) renderer2d?
  (make-renderer2d (x-axis-render-proc y ticks?) null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

(defproc (y-axis [x real? 0] [#:ticks? ticks? boolean? (y-axis-ticks?)]) renderer2d?
  (make-renderer2d (y-axis-render-proc x ticks?) null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

(defproc (axes [x real? 0] [y real? 0]
               [#:x-ticks? x-ticks? boolean? (x-axis-ticks?)]
               [#:y-ticks? y-ticks? boolean? (y-axis-ticks?)]) renderer2d?
  (make-renderer2d (λ (area)
                     ((x-axis-render-proc x x-ticks?) area)
                     ((y-axis-render-proc y y-ticks?) area))
                   null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

;; ===================================================================================================
;; Polar axes

(define ((polar-axes-render-proc num ticks?) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  
  (define step (/ (* 2 pi) num))
  (define θs (build-list num (λ (n) (* n step))))
  
  (send area set-minor-pen)
  (let ([r  (* 2 (max (- x-min) x-max (- y-min) y-max))])
    (for ([θ  (in-list θs)])
      (send area put-line (vector 0 0) (vector (* r (cos θ)) (* r (sin θ))))))
  
  (define ticks (remove-duplicates (map (λ (t) (abs (tick-p t)))
                                        (send area get-x-ticks))))
  
  (send area set-minor-pen 'long-dash)
  (for ([r  (in-list ticks)])
    (define pts (for/list ([θ  (in-list (linear-seq 0 (* 2 pi) 100))])
                  (vector (* r (cos θ)) (* r (sin θ)))))
    (send area put-lines pts))
  
  empty)

(defproc (polar-axes [#:number num (integer>=/c 1) (polar-axes-number)]
                     [#:ticks? ticks? boolean? (polar-axes-ticks?)]
                     ) renderer2d?
  (make-renderer2d (polar-axes-render-proc num ticks?)
                   null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

;; ===================================================================================================
;; Grid

(define ((x-tick-lines-render-proc) area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define x-ticks (send area get-x-ticks))
  
  (send area set-pen (plot-foreground) (* 1/2 (plot-pen-width)) 'long-dash)
  (for ([t  (in-list x-ticks)])
    (match-define (tick x _ major?) t)
    (send area put-line (vector x y-min) (vector x y-max)))
  
  empty)

(define ((y-tick-lines-render-proc) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-ticks (send area get-y-ticks))
  
  (send area set-pen (plot-foreground) (* 1/2 (plot-pen-width)) 'long-dash)
  (for ([t  (in-list y-ticks)])
    (match-define (tick y _ major?) t)
    (send area put-line (vector x-min y) (vector x-max y)))
  
  empty)

(defproc (x-tick-lines) renderer2d?
  (make-renderer2d (x-tick-lines-render-proc) null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

(defproc (y-tick-lines) renderer2d?
  (make-renderer2d (y-tick-lines-render-proc) null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

(defproc (tick-grid) renderer2d?
  (make-renderer2d (λ (area)
                     ((x-tick-lines-render-proc) area)
                     ((y-tick-lines-render-proc) area))
                   null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

;; ===================================================================================================
;; Labeled points

(define (format-x-coordinate x area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (format "~a" (real->string/trunc x (digits-for-range x-min x-max))))

(define (format-y-coordinate y area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (format "~a" (real->string/trunc y (digits-for-range y-min y-max))))

(define (format-coordinate v area)
  (match-define (vector x y) v)
  (format "(~a,~a)" (format-x-coordinate x area) (format-y-coordinate y area)))

(define ((label-render-proc label v color size anchor angle point-size alpha) area)
  (let ([label  (if label label (format-coordinate v area))])
    (send area set-alpha alpha)
    ; label
    (send area set-text-foreground color)
    (send area set-font-size size)
    (send area put-text (string-append " " label " ") v anchor angle #:outline? #t)
    ; point
    (send area set-pen color 1 'solid)
    (send area put-glyphs (list v) 'fullcircle point-size))
  
  empty)

(defproc (point-label
          [v (vector/c real? real?)] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size font-size/c (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (real>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (match-define (vector x y) v)
  (make-renderer2d
   (label-render-proc label v color size anchor angle point-size alpha)
   null-2d-ticks-fun null-2d-bounds-fun
   x x y y))

(defproc (parametric-label
          [f (real? . -> . (vector/c real? real?))]
          [t real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size font-size/c (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (real>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (match f
                 [(vector fx fy)  (vector (fx t) (fy t))]
                 [(? procedure?)  (f t)])
               label #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))

(defproc (polar-label
          [f (real? . -> . real?)] [θ real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size font-size/c (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (real>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label ((2d-polar->parametric f) θ) label
               #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))

(defproc (function-label
          [f  (real? . -> . real?)] [x real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size font-size/c (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (real>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (vector x (f x)) label
               #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))

(defproc (inverse-label
          [f  (real? . -> . real?)] [y real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size font-size/c (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (real>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (vector (f y) y) label
               #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))
