#lang racket/base

(require racket/gui racket/flonum
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/marching-squares.rkt"
         "../common/ticks.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (except-out (all-defined-out)
                     add-surface3d add-contour3d add-shade3d))

(define surface3d-samples (make-parameter 41))

(define (get-surface3d-samples)
  (ceiling (* (if (plot3d-animating?) 1/4 1) (surface3d-samples))))

(define surface3d-color (make-parameter "white"))
(define surface3d-line-color (make-parameter "black"))
(define surface3d-line-width (make-parameter 1/3))
(define surface3d-line-style (make-parameter 'solid))
(define surface3d-alpha (make-parameter 1.0))

(define contour3d-levels (make-parameter 'auto))
(define contour3d-line-color (make-parameter "darkred"))
(define contour3d-line-width (make-parameter 1))
(define contour3d-line-style (make-parameter 'solid))
(define contour3d-alpha (make-parameter 1.0))

(define (default-shade3d-color-function z-min z-max num)
  (list "white" '(224 240 255)))

(define shade3d-color-function (make-parameter default-shade3d-color-function))
(define shade3d-line-color (make-parameter "black"))
(define shade3d-line-width (make-parameter 1/3))
(define shade3d-line-style (make-parameter 'solid))
(define shade3d-alpha (make-parameter 1.0))

;; =============================================================================
;; surface3d

(define ((add-surface3d f color line-color line-width line-style alpha) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define-values (xs ys zss fz-min fz-max)
    (sample-2d-function f x-min x-max y-min y-max (get-surface3d-samples)))
  (send area set-alpha alpha)
  (send area set-brush color 'solid #;(if (plot3d-animating?) 'transparent 'solid))
  (send area set-pen line-color line-width line-style)
  (for ([ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [j   (in-naturals)]
        #:when #t  ; nest the next loop
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [i   (in-naturals)])
    (define z1 (flvector-ref (vector-ref zss j) i))
    (define z2 (flvector-ref (vector-ref zss j) (add1 i)))
    (define z3 (flvector-ref (vector-ref zss (add1 j)) i))
    (define z4 (flvector-ref (vector-ref zss (add1 j)) (add1 i)))
    (define v1 (vector xa ya z1))
    (define v2 (vector xb ya z2))
    (define v3 (vector xa yb z3))
    (define v4 (vector xb yb z4))
    (send area add-polygon (list v1 v2 v4 v3))))

(define (surface3d
         f [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
         #:color [color (surface3d-color)]
         #:line-color [line-color (surface3d-line-color)]
         #:line-width [line-width (surface3d-line-width)]
         #:line-style [line-style (surface3d-line-style)]
         #:alpha [alpha (surface3d-alpha)])
  (define-values (xs ys zss fz-min fz-max)
    (sample-2d-function f x-min x-max y-min y-max (surface3d-samples)))
  (let* ([z-min  (if (and (not z-min) x-min x-max y-min y-max)
                     fz-min
                     z-min)]
         [z-max  (if (and (not z-max) x-min x-max y-min y-max)
                     fz-max
                     z-max)])
    (renderer3d (add-surface3d f color line-color line-width line-style alpha)
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                x-min x-max y-min y-max z-min z-max)))

;; =============================================================================
;; contour3d

(define ((add-contour3d f line-color line-width line-style alpha) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define z-min (send area get-z-min))
  (define z-max (send area get-z-max))
  
  (define-values (xs ys zss fz-min fz-max)
    (sample-2d-function f x-min x-max y-min y-max (get-surface3d-samples)))
  
  (define levels
    (case (contour3d-levels)
      [(auto)  (define ticks
                 ((default-range->ticks (plot3d-tick-skip)) z-min z-max))
               (map tick-p ticks)]
      [else    (define num (sub1 (contour3d-levels)))
               (take (rest (real-seq z-min z-max (+ 2 num))) num)]))
  
  (send area set-alpha alpha)
  (send area set-pen line-color line-width line-style)
  (for ([z  (in-list levels)]
        #:when #t
        [ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [j   (in-naturals)]
        #:when #t  ; nest the next loop
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [i   (in-naturals)])
    (define z1 (flvector-ref (vector-ref zss j) i))
    (define z2 (flvector-ref (vector-ref zss j) (add1 i)))
    (define z3 (flvector-ref (vector-ref zss (add1 j)) (add1 i)))
    (define z4 (flvector-ref (vector-ref zss (add1 j)) i))
    (define lines (heights->lines (exact->inexact z) z1 z2 z3 z4))
    (for/list ([line  (in-list lines)])
      (match-define (vector u1 v1 u2 v2) line)
      (define a (vector (denormalize-t u1 xb xa) (denormalize-t v1 yb ya) z))
      (define b (vector (denormalize-t u2 xb xa) (denormalize-t v2 yb ya) z))
      (define c (center-coord (list (vector xa ya z1) (vector xb ya z2)
                                    (vector xa yb z3) (vector xb yb z4))))
      (send area add-line/center a b c))))

(define (contour3d
         f [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
         #:line-color [line-color (contour3d-line-color)]
         #:line-width [line-width (contour3d-line-width)]
         #:line-style [line-style (contour3d-line-style)]
         #:alpha [alpha (contour3d-alpha)])
  (define-values (xs ys zss fz-min fz-max)
    (sample-2d-function f x-min x-max y-min y-max (surface3d-samples)))
  (let* ([z-min  (if (and (not z-min) x-min x-max y-min y-max)
                     fz-min
                     z-min)]
         [z-max  (if (and (not z-max) x-min x-max y-min y-max)
                     fz-max
                     z-max)])
    (renderer3d (add-contour3d f line-color line-width line-style alpha)
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                x-min x-max y-min y-max z-min z-max)))

;; =============================================================================
;; shade3d

(define ((add-shade3d f color-function line-color line-width line-style alpha)
         area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define z-min (send area get-z-min))
  (define z-max (send area get-z-max))
  
  (define-values (xs ys zss fz-min fz-max)
    (sample-2d-function f x-min x-max y-min y-max (get-surface3d-samples)))
  
  (define levels
    (case (contour3d-levels)
      [(auto)
       (define levels
         (map tick-p ((default-range->ticks (plot3d-tick-skip)) z-min z-max)))
       (let* ([levels  (if (= (first levels) z-min) levels (cons z-min levels))]
              [levels  (if (= (last levels) z-max) levels
                           (append levels (list z-max)))])
         levels)]
      [else    (real-seq z-min z-max (+ 1 (contour3d-levels)))]))
  
  (define colors (color-function z-min z-max (sub1 (length levels))))
  
  (send area set-alpha alpha)
  (for ([za  (in-list levels)]
        [zb  (in-list (rest levels))]
        [color  (in-cycle colors)])
    (send area set-brush color 'solid #;(if (plot3d-animating?) 'transparent 'solid))
    (send area set-pen line-color line-width line-style)
    (for ([ya  (in-list ys)]
          [yb  (in-list (rest ys))]
          [j   (in-naturals)]
          #:when #t  ; nest the next loop
          [xa  (in-list xs)]
          [xb  (in-list (rest xs))]
          [i   (in-naturals)])
      (define z1 (flvector-ref (vector-ref zss j) i))
      (define z2 (flvector-ref (vector-ref zss j) (add1 i)))
      (define z3 (flvector-ref (vector-ref zss (add1 j)) (add1 i)))
      (define z4 (flvector-ref (vector-ref zss (add1 j)) i))
      (define facet
        (heights->mid-polys (exact->inexact za) (exact->inexact zb)
                            z1 z2 z3 z4))
      (for ([poly0  (in-list facet)])
        (define rect (list (vector xa ya z1) (vector xb ya z2)
                           (vector xb yb z3) (vector xa yb z4)))
        (define c (center-coord rect))
        (define poly
          (cond
            [(equal? poly0 'full)  rect]
            [else
             (for/list ([uv  (in-list poly0)])
               (match-define (vector u v z) uv)
               (vector (denormalize-t u xb xa) (denormalize-t v yb ya) z))]))
        (send area add-polygon/center poly c)))))

(define (shade3d
         f [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
         #:colors [color-function (shade3d-color-function)]
         #:line-color [line-color (shade3d-line-color)]
         #:line-width [line-width (shade3d-line-width)]
         #:line-style [line-style (shade3d-line-style)]
         #:alpha [alpha (shade3d-alpha)])
  (define-values (xs ys zss fz-min fz-max)
    (sample-2d-function f x-min x-max y-min y-max (surface3d-samples)))
  (let* ([z-min  (if (and (not z-min) x-min x-max y-min y-max)
                     fz-min
                     z-min)]
         [z-max  (if (and (not z-max) x-min x-max y-min y-max)
                     fz-max
                     z-max)])
    (renderer3d (add-shade3d f color-function
                             line-color line-width line-style alpha)
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                (default-range->ticks (plot3d-tick-skip))
                x-min x-max y-min y-max z-min z-max)))
