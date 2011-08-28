#lang racket/base

(require racket/gui racket/math racket/flonum
         "../common/math.rkt"
         "../common/color.rkt"
         "../common/marching-squares.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (except-out (all-defined-out)
                     add-contour
                     (struct-out fast-rect) add-shade))

(defparam contour-levels levels positive-integer/c 10)
(defparam contour-samples samples positive-integer/c 51)
(defparam contour-color color plot-color/c "black")
(defparam contour-width width nonnegative-real/c 1)
(defparam contour-style style pen-style/c 'solid)
(defparam contour-alpha alpha (real-in 0 1) 1)

(defproc (default-shade-color-function
           [z-min real?] [z-max real?] [levels positive-integer/c]
           ) (listof plot-color/c)
  (color-seq* (list "blue" "white" "red") levels))

(defparam shade-color-function function
  color-function/c default-shade-color-function)

;; =============================================================================
;; Drawing contour lines using marching squares

(define ((add-contour f samples color width style alpha) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  
  (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
  (define-values (z-min z-max)
    (let ([zs  (2d-sample->list zss)])
      (values (apply regular-min zs) (apply regular-max zs))))
  
  (define levels (sub1 (contour-levels)))
  (define ticks (take (rest (real-seq z-min z-max (+ 2 levels))) levels))
  
  (send area set-alpha alpha)
  (send area set-pen color width style)
  (for ([t   (in-list ticks)]
        #:when #t  ; nest the next loop
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
      (send area add-line 
            (vector (denormalize-t u1 xb xa) (denormalize-t v1 yb ya))
            (vector (denormalize-t u2 xb xa) (denormalize-t v2 yb ya))))))

(define (contour f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                 #:samples [samples (contour-samples)]
                 #:color [color (contour-color)]
                 #:width [width (contour-width)]
                 #:style [style (contour-style)]
                 #:alpha [alpha (contour-alpha)])
  (define g (memoize-sample-2d-function f))
  (make-renderer2d (add-contour g samples color width style alpha)
                   (default-range->ticks (plot2d-tick-skip))
                   (default-range->ticks (plot2d-tick-skip))
                   values
                   x-min x-max y-min y-max))

;; =============================================================================
;; Drawing contour areas using marching squares

(struct fast-rect (xa ya xb yb) #:transparent)

(define ((add-shade f samples color-function) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  
  (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
  (define-values (z-min z-max)
    (let ([zs  (2d-sample->list zss)])
      (values (apply regular-min zs) (apply regular-max zs))))
  
  (define levels (real-seq z-min z-max (+ 1 (contour-levels))))
  (define colors (color-function z-min z-max (contour-levels)))
  
  (send area set-alpha 1.0)
  (for ([za     (in-list levels)]
        [zb     (in-list (rest levels))]
        [color  (in-cycle colors)])
    (send area set-brush color 'solid)
    (send area set-pen color 1.5 'solid)
    (define polys
      (append*
       (for/list ([ya  (in-list ys)]
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
         (for/list ([poly  (in-list facet)])
           (cond [(equal? poly 'full)  (fast-rect xa ya xb yb)]
                 [else  (for/list ([uv  (in-list poly)])
                          (match-define (vector u v _) uv)
                          (vector (denormalize-t u xb xa)
                                  (denormalize-t v yb ya)))])))))
    #;; the straightforward way:
    (for ([poly  (in-list polys)])
      (match poly
        [(fast-rect xa ya xb yb)
         (send area add-rectangle (vector xa ya) (vector xb yb))]
        [_  (send area add-polygon poly)]))
    ; the less straightforward way, which saves GDI calls by stitching
    ; neighboring rectangles together
    (define last-rect
      (for/fold ([last-rect #f]) ([poly  (in-list polys)])
        (match poly
          [(fast-rect xa ya xb yb)
           ; we have a fast-rect: see what last-rect was
           (match last-rect
             [(fast-rect lxa lya lxb lyb)
              ; the last-rect is a fast-rect: see if poly can extend it
              (cond [(= xa lxb)
                     ; adjacent: extend last-rect
                     (fast-rect lxa lya xb yb)]
                    [else
                     ; not adjacent: draw last-rect, make poly the new one
                     (send area add-rectangle (vector lxa lya) (vector lxb lyb))
                     poly])]
             [_
              ; no last-rect: make poly the new one
              poly])]
          [_
           ; we have a polygon: draw it
           (send area add-polygon poly)
           ; do we have a last-rect?
           (match last-rect
             [(fast-rect lxa lya lxb lyb)
              ; yes: draw it, too
              (send area add-rectangle (vector lxa lya) (vector lxb lyb))]
             [_
              ; no: we're done, and we have no last-rect
              #f])])))
    ; draw the last-rect if we have one
    (match last-rect
      [(fast-rect lxa lya lxb lyb)
       (send area add-rectangle (vector lxa lya) (vector lxb lyb))]
      [_  (void)])))

(define (shade f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
               #:samples [samples (contour-samples)]
               #:colors [color-function (shade-color-function)])
  (define g (memoize-sample-2d-function f))
  (make-renderer2d (add-shade g samples color-function)
                   (default-range->ticks (plot2d-tick-skip))
                   (default-range->ticks (plot2d-tick-skip))
                   values
                   x-min x-max y-min y-max))
