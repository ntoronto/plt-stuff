#lang racket/base

(require racket/gui racket/flonum
         "../common/utils.rkt"
         "../common/marching-squares.rkt"
         "area.rkt"
         "renderer.rkt")

(provide surface3d-color surface3d-line-color surface3d-line-width
         contour3d-levels contour3d-line-color contour3d-line-width
         surface3d contour3d shade3d)

(define surface3d-color (make-parameter '(255 255 255)))
(define surface3d-line-color (make-parameter '(0 0 0)))
(define surface3d-line-width (make-parameter 1/3))

(define contour3d-levels (make-parameter 10))    
(define contour3d-line-color (make-parameter '(255 0 0)))
(define contour3d-line-width (make-parameter 1))

;; =============================================================================
;; surface3d

(define ((draw-surface3d xs ys zss color line-color line-width) area)
  (let ([x-min  (send area get-x-min)]
        [x-max  (send area get-x-max)]
        [y-min  (send area get-y-min)]
        [y-max  (send area get-y-max)]
        [z-min  (send area get-z-min)]
        [z-max  (send area get-z-max)])
    (send area set-brush (color->color% color) 'solid)
    (send area set-pen (color->color% line-color) line-width 'solid)
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
      (send area draw-polygon (list v1 v2 v4 v3)))))

(define (surface3d f x-min x-max y-min y-max
                   #:color [color (surface3d-color)]
                   #:line-color [line-color (surface3d-line-color)]
                   #:line-width [line-width (surface3d-line-width)]
                   #:z-min [z-min #f] #:z-max [z-max #f])
  (define-values (xs ys zss fz-min fz-max)
      (sample-2d-function f x-min x-max y-min y-max (plot3d-samples)))
  (let ([x-min  (if x-min x-min (apply min xs))]
        [x-max  (if x-max x-max (apply max xs))]
        [y-min  (if y-min y-min (apply min ys))]
        [y-max  (if y-max y-max (apply max ys))]
        [z-min  (if z-min z-min fz-min)]
        [z-max  (if z-max z-max fz-max)])
    (renderer3d (draw-surface3d xs ys zss color line-color line-width)
                x-min x-max y-min y-max z-min z-max)))

;; =============================================================================
;; contour3d

(define ((draw-contour3d xs ys zss line-color line-width) area)
  (let ([x-min  (send area get-x-min)]
        [x-max  (send area get-x-max)]
        [y-min  (send area get-y-min)]
        [y-max  (send area get-y-max)]
        [z-min  (send area get-z-min)]
        [z-max  (send area get-z-max)])
    (send area set-pen (color->color% line-color) line-width 'solid)
    
    (define levels (sub1 (contour3d-levels)))
    (define ticks (take (rest (real-seq z-min z-max (+ 2 levels))) levels))
    
    (for ([t  (in-list ticks)]
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
      (define lines (heights->lines (exact->inexact t) z1 z2 z3 z4))
      (for/list ([line  (in-list lines)])
        (match-define (vector u1 v1 u2 v2) line)
        (define a (vector (denormalize-t u1 xb xa) (denormalize-t v1 yb ya) t))
        (define b (vector (denormalize-t u2 xb xa) (denormalize-t v2 yb ya) t))
        (define c (center-coord (list (vector xa ya z1) (vector xb ya z2)
                                      (vector xa yb z3) (vector xb yb z4))))
        (send area draw-line/center a b c)))))

(define (contour3d f x-min x-max y-min y-max
                   #:line-color [line-color (contour3d-line-color)]
                   #:line-width [line-width (contour3d-line-width)]
                   #:z-min [z-min #f] #:z-max [z-max #f])
  (define-values (xs ys zss fz-min fz-max)
      (sample-2d-function f x-min x-max y-min y-max (plot3d-samples)))
  (let ([x-min  (if x-min x-min (apply min xs))]
        [x-max  (if x-max x-max (apply max xs))]
        [y-min  (if y-min y-min (apply min ys))]
        [y-max  (if y-max y-max (apply max ys))]
        [z-min  (if z-min z-min fz-min)]
        [z-max  (if z-max z-max fz-max)])
    (renderer3d (draw-contour3d xs ys zss line-color line-width)
                x-min x-max y-min y-max z-min z-max)))

;; =============================================================================
;; shade3d

(define ((draw-shade3d xs ys zss line-color line-width) area)
  (let ([x-min  (send area get-x-min)]
        [x-max  (send area get-x-max)]
        [y-min  (send area get-y-min)]
        [y-max  (send area get-y-max)]
        [z-min  (send area get-z-min)]
        [z-max  (send area get-z-max)])
    (define ticks (real-seq z-min z-max (+ 1 (contour3d-levels))))
    
    (for ([za  (in-list ticks)]
          [zb  (in-list (rest ticks))]
          [k   (in-naturals)])
      (define g (if (zero? (remainder k 2)) 224 255))
      (send area set-brush (color->color% (list g g g)) 'solid)
      (send area set-pen (color->color% line-color) line-width 'solid)
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
          (send area draw-polygon/center poly c))))))

(define (shade3d f x-min x-max y-min y-max
                 #:line-color [line-color (surface3d-line-color)]
                 #:line-width [line-width (surface3d-line-width)]
                 #:z-min [z-min #f] #:z-max [z-max #f])
  (define-values (xs ys zss fz-min fz-max)
      (sample-2d-function f x-min x-max y-min y-max (plot3d-samples)))
  (let ([x-min  (if x-min x-min (apply min xs))]
        [x-max  (if x-max x-max (apply max xs))]
        [y-min  (if y-min y-min (apply min ys))]
        [y-max  (if y-max y-max (apply max ys))]
        [z-min  (if z-min z-min fz-min)]
        [z-max  (if z-max z-max fz-max)])
    (renderer3d (draw-shade3d xs ys zss line-color line-width)
                x-min x-max y-min y-max z-min z-max)))
