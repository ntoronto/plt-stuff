#lang racket/base

(require racket/gui racket/math racket/flonum
         "../common/utils.rkt"
         "../common/marching-squares.rkt"
         "../common/ticks.rkt"
         "area.rkt"
         "renderer.rkt")

(provide contour-levels contour-samples
         contour-line-color contour-line-width
         contour shade)

(define contour-levels (make-parameter 10))
(define contour-samples (make-parameter 50))
(define contour-line-color (make-parameter "red"))
(define contour-line-width (make-parameter 1))

;; =============================================================================
;; Drawing contour lines using marching squares

(define ((draw-contour f color width x-min x-max y-min y-max) area)
  (let ([x-min  (if x-min x-min (send area get-x-min))]
        [x-max  (if x-max x-max (send area get-x-max))]
        [y-min  (if y-min y-min (send area get-y-min))]
        [y-max  (if y-max y-max (send area get-y-max))])
    (define-values (xs ys zss z-min z-max)
      (sample-2d-function f x-min x-max y-min y-max (contour-samples)))
    
    (define levels (sub1 (contour-levels)))
    (define ticks (take (rest (real-seq z-min z-max (+ 2 levels))) levels))
    
    (send area set-pen color width 'solid)
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
        (send area draw-line 
              (vector (denormalize-t u1 xb xa) (denormalize-t v1 yb ya))
              (vector (denormalize-t u2 xb xa) (denormalize-t v2 yb ya)))))))

(define (contour f
                 #:color [color (contour-line-color)]
                 #:width [width (contour-line-width)]
                 #:x-min [x-min #f] #:x-max [x-max #f]
                 #:y-min [y-min #f] #:y-max [y-max #f])
  (renderer2d (draw-contour f color width x-min x-max y-min y-max)
              x-min x-max y-min y-max))

;; =============================================================================
;; Drawing contour areas using marching squares

(struct fast-rect (xa ya xb yb) #:transparent)

(define ((draw-shade f x-min x-max y-min y-max) area)
  (let ([x-min  (if x-min x-min (send area get-x-min))]
        [x-max  (if x-max x-max (send area get-x-max))]
        [y-min  (if y-min y-min (send area get-y-min))]
        [y-max  (if y-max y-max (send area get-y-max))])
    (define-values (xs ys zss z-min z-max)
      (sample-2d-function f x-min x-max y-min y-max (contour-samples)))

    (define ticks (real-seq z-min z-max (+ 1 (contour-levels))))
    
    (for ([za    (in-list ticks)]
          [zb    (in-list (rest ticks))]
          [gray  (in-list (real-seq 0 255 (contour-levels)))])
      (define color
        (make-object color% (round gray) (round gray) (round gray)))
      (send area set-brush color 'solid)
      (send area set-pen color 1 'solid)
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
           (send area draw-rectangle
                 (vector xa ya) (vector (- xb xa) (- yb ya)))]
          [_  (send area draw-polygon poly)]))
      ; all the "last-rect" stuff stitches adjacent rectangles together,
      ; which can drastically reduce the number of GDI calls
      ; stitching rectangles makes test plots run about 1.5x faster
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
                       (send area draw-rectangle
                             (vector lxa lya)
                             (vector (- lxb lxa) (- lyb lya)))
                       poly])]
               [_
                ; no last-rect: make poly the new one
                poly])]
            [_
             ; we have a polygon: draw it
             (send area draw-polygon poly)
             ; do we have a last-rect?
             (match last-rect
               [(fast-rect lxa lya lxb lyb)
                ; yes: draw it, too
                (send area draw-rectangle
                      (vector lxa lya)
                      (vector (- lxb lxa) (- lyb lya)))]
               [_
                ; no: we're done, and we have no last-rect
                #f])])))
      ; draw the last-rect if we have one
      (match last-rect
        [(fast-rect lxa lya lxb lyb)
         (send area draw-rectangle
               (vector lxa lya)
               (vector (- lxb lxa) (- lyb lya)))]
        [_  (void)]))))

(define (shade f
               #:x-min [x-min #f] #:x-max [x-max #f]
               #:y-min [y-min #f] #:y-max [y-max #f])
  (renderer2d (draw-shade f x-min x-max y-min y-max)
              x-min x-max y-min y-max))
