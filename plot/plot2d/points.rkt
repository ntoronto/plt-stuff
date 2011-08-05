#lang racket/base

(require racket/gui
         "../common/utils.rkt"
         "area.rkt"
         "renderer.rkt")

(provide point-sym point-color points
         field-x-points field-y-points field-arrow-color
         field-arrow-width field-arrow-style vector-field)

(define point-sym (make-parameter 'fullsquare))
(define point-color (make-parameter "red"))

(define field-x-points (make-parameter 20))
(define field-y-points (make-parameter 20))
(define field-arrow-color (make-parameter "red"))
(define field-arrow-width (make-parameter 1))
(define field-arrow-style (make-parameter 'scaled))

(define (draw-circle area x y r)
  (let ([x   (+ x (send area dc->plot/x-size 1/2))]
        [y   (- y (send area dc->plot/y-size 1/2))]
        [xr  (send area dc->plot/x-size r)]
        [yr  (send area dc->plot/y-size r)])
    (send area draw-ellipse
          (vector (- x xr) (- y yr))
          (vector (* 2 xr) (* 2 yr)))))

(define (draw-polygon area sides x y r start-angle)
  (define xr (send area dc->plot/x-size r))
  (define yr (send area dc->plot/y-size r))
  (define rads (real-seq start-angle (+ start-angle (* 2 pi)) (+ 1 sides)))
  (for ([rad1  (in-list rads)]
        [rad2  (in-list (rest rads))])
    (send area draw-line
          (vector (+ x (* (cos rad1) xr)) (+ y (* (sin rad1) yr)))
          (vector (+ x (* (cos rad2) xr)) (+ y (* (sin rad2) yr))))))

(define (draw-flare area sticks x y r start-angle)
  (define xr (send area dc->plot/x-size r))
  (define yr (send area dc->plot/y-size r))
  (define step (/ (* 2 pi) sticks))
  (define rads (build-list sticks (λ (n) (+ start-angle (* n step)))))
  (for ([rad  (in-list rads)])
    (send area draw-line
          (vector x y)
          (vector (+ x (* (cos rad) xr)) (+ y (* (sin rad) yr))))))

(define (draw-arrow area x y r rad)
  (define xr (send area dc->plot/x-size r))
  (define yr (send area dc->plot/y-size r))
  (define xhr (send area dc->plot/x-size (min 5 (* 4/5 r))))
  (define yhr (send area dc->plot/y-size (min 5 (* 4/5 r))))
  (define head-rad (* 1/6 pi))
  (define head-x (+ x (* (cos rad) xr)))
  (define head-y (+ y (* (sin rad) yr)))
  (define head (vector head-x head-y))
  (define tail (vector (- x (* (cos rad) xr)) (- y (* (sin rad) yr))))
  (send area draw-line head tail)
  (send area draw-line head (vector (- head-x (* (cos (+ rad head-rad)) xhr))
                                    (- head-y (* (sin (+ rad head-rad)) yhr))))
  (send area draw-line head (vector (- head-x (* (cos (- rad head-rad)) xhr))
                                    (- head-y (* (sin (- rad head-rad)) yhr)))))

(define ((draw-points xys sym color) area)
  (match sym
    [(? string?)  (send area set-text-foreground color)
                  (for ([xy  (in-list xys)])
                    (send area draw-text-point sym xy))]
    [(? symbol?)
     (send area set-pen color 1 'solid)
     (send area set-brush color 'transparent)
     (for ([xy  (in-list xys)])
       (match-define (vector x y) xy)
       (case sym
         [(pixel dot)   (send area set-pen color 3/2 'solid)
                        (send area draw-point xy)]
         [(bullet)      (send area set-brush color 'solid)
                        (draw-circle area x y 2)]
         [(circle1)     (draw-circle area x y 3/2)]
         [(circle2)     (draw-circle area x y 4/2)]
         [(circle3)     (draw-circle area x y 5/2)]
         [(circle circle4)  (draw-circle area x y 6/2)]
         [(circle5)     (draw-circle area x y 8/2)]
         [(circle6)     (draw-circle area x y 10/2)]
         [(circle7)     (draw-circle area x y 12/2)]
         [(circle8)     (draw-circle area x y 14/2)]
         [(plus)        (draw-flare area 4 x y 3 0)]
         [(asterisk)    (draw-flare area 6 x y 3 0)]
         [(times)       (draw-flare area 4 x y 3 (* 1/4 pi))]
         [(square)      (draw-polygon area 4 x y 3 (* 1/4 pi))]
         [(diamond)     (draw-polygon area 4 x y 3 0)]
         [(triangle)    (draw-polygon area 3 x y 3 (* 1/2 pi))]
         [(oplus)       (draw-circle area x y 4)
                        (draw-flare area 4 x y 3 0)]
         [(odot)        (draw-circle area x y 4)
                        (send area set-pen color 3/2 'solid)
                        (send area draw-point xy)]
         [(otimes)      (draw-circle area x y 4)
                        (draw-flare area 4 x y 3 (* 1/4 pi))]
         [(rightarrow)  (draw-arrow area x y 4 0)]
         [(leftarrow)   (draw-arrow area x y 4 pi)]
         [(uparrow)     (draw-arrow area x y 4 (* 1/2 pi))]
         [(downarrow)   (draw-arrow area x y 4 (* 3/2 pi))]
         [else     (error 'points "unknown point symbol ~e" sym)]))]
    [_  (raise-type-error 'draw-points "string or symbol" sym)]))

(define (points xys
                #:sym [sym (point-sym)]
                #:color [color (point-color)]
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f])
  (define xs (map (λ (xy) (vector-ref xy 0)) xys))
  (define ys (map (λ (xy) (vector-ref xy 1)) xys))
  (let ([x-min  (if x-min x-min (apply min xs))]
        [x-max  (if x-max x-max (apply max xs))]
        [y-min  (if y-min y-min (apply min ys))]
        [y-max  (if y-max y-max (apply max ys))])
    (renderer2d (draw-points xys sym color)
                x-min x-max y-min y-max)))

(define ((draw-vectors f color width style x-min x-max y-min y-max) area)
  (let ([x-min  (if x-min x-min (send area get-x-min))]
        [x-max  (if x-max x-max (send area get-x-max))]
        [y-min  (if y-min y-min (send area get-y-min))]
        [y-max  (if y-max y-max (send area get-y-max))])
    (define xs (real-seq x-min x-max (field-x-points)))
    (define ys (real-seq y-min y-max (field-y-points)))
    (define xys (for*/list ([x  (in-list xs)] [y  (in-list ys)])
                  (vector x y)))
    (define-values (dxs dys)
      (let ([dxys  (map f xys)])
        (values (map (λ (xy) (exact->inexact (vector-ref xy 0))) dxys)
                (map (λ (xy) (exact->inexact (vector-ref xy 1))) dxys))))
    (define rads (map (λ (dx dy) (flatan2 dy dx)) dxs dys))
    (define mags
      (cond [(symbol=? style 'normalized)
             (define box-x-size
               (send area plot->dc/x-size
                     (/ (- x-max x-min) (sub1 (field-x-points)))))
             (define box-y-size
               (send area plot->dc/y-size
                     (/ (- y-max y-min) (sub1 (field-y-points)))))
             (define box-size (min box-x-size box-y-size))
             (build-list (length dxs) (λ _ box-size))]
            [(symbol=? style 'real)
             (map (λ (dx dy)
                    (sqrt (+ (sqr (send area plot->dc/x-size dx))
                             (sqr (send area plot->dc/y-size dy)))))
                  dxs dys)]
            [(symbol=? style 'scaled)
             (define dx-max (apply max (map abs dxs)))
             (define dy-max (apply max (map abs dys)))
             (define box-x-size (/ (- x-max x-min) (sub1 (field-x-points))))
             (define box-y-size (/ (- y-max y-min) (sub1 (field-y-points))))
             (define scale (min (/ box-x-size dx-max) (/ box-y-size dy-max)))
             (map (λ (dx dy)
                    (sqrt (+ (sqr (* scale (send area plot->dc/x-size dx)))
                             (sqr (* scale (send area plot->dc/y-size dy))))))
                  dxs dys)]))
    (send area set-pen color width 'solid)
    (for ([xy   (in-list xys)]
          [rad  (in-list rads)]
          [mag  (in-list mags)])
      (match-define (vector x y) xy)
      (draw-arrow area x y (/ mag 2) rad))))

(define (vector-field f
                      #:color [color (field-arrow-color)]
                      #:width [width (field-arrow-width)]
                      #:style [style (field-arrow-style)]
                      #:x-min [x-min #f] #:x-max [x-max #f]
                      #:y-min [y-min #f] #:y-max [y-max #f])
  (renderer2d (draw-vectors f color width style x-min x-max y-min y-max)
              x-min x-max y-min y-max))
