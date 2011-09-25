#lang racket

(require racket/flonum
         "../plot2d.rkt"
         "../common/math.rkt"
         "../common/transform.rkt"
         "../common/draw.rkt")

;(plot2d-new-window? #t)

(parameterize ([plot2d-x-transform  log-transform]
               #;[plot2d-y-transform  log-transform])
  (time
   (plot2d (list (function (λ (x) x) 0.1 10 #:samples 2 #:label "y = x")
                 (polar-axes))
           #:title "Nonlinear scaling" #:x-label "x axis" #:y-label "y-axis")))

(define (degrees->point θ [r 1])
  (vector (* r (cos (degrees->radians θ)))
          (* r (sin (degrees->radians θ)))))

(plot2d (list (lines (list (degrees->point 0)
                           (degrees->point 120)
                           (degrees->point 180 0)
                           (degrees->point 240)
                           (degrees->point 0)))
              (polar (λ (θ) 1) #:color 0)))

;(plot2d-x-transform cbrt-transform)
;(plot2d-y-transform cbrt-transform)
;(plot2d-x-transform (hand-drawn-x-transform 300))
;(plot2d-y-transform (hand-drawn-y-transform 300))

(plot2d (vector-field (λ (x y) (vector (- y) x))
                      -2 2 -1 4 #:arrow-scale 1/12))

(time
 (plot2d (list (lines (for/list ([i  (in-range 6)])
                        (degrees->point (* 2 72 i))))
               (polar (λ (θ) 1) #:color 0)
               (polar (λ (θ) 0.38) #:color 0))))

(time
 (plot2d (list (lines (for/list ([i  (in-range 4)])
                        (degrees->point (* 120 i))))
               (lines (for/list ([i  (in-range 4)])
                        (degrees->point (+ 60 (* 120 i)))))
               (polar (λ (θ) 1) #:color 0)
               (polar (λ (θ) 0.5) #:color 0))))

(time
 (define xs (build-list 100 (λ _ (random))))
 (define ys (build-list 100 (λ _ (random))))
 (plot2d (points (map vector xs ys) #:symbol 'full8star)))

(time
 (plot2d (vector-field (λ (x y) (vector +nan.0 +nan.0)))
         #:x-min -2 #:x-max 2 #:y-min -2 #:y-max 2))

#;; high-contrast white-on-black:
(begin
  (plot2d-foreground "white")
  (plot2d-background "black")
  (point-color "white")
  (histogram-line-color "white")
  (histogram-fill-color "black")
  (contour-color "white"))

;; an exact rational function and a floating-point function
;; the plot of the exact rational function's graph should be smooth
(time
 (plot2d (list (function (λ (x) (+ x 1)) #:label "Exact")
               (function (λ (x) (- #e100000000000000.8 (sin x)))
                         #:color 2 #:label "Inexact"))
         #:x-min #e100000000000000.0 #:x-max #e100000000000000.1
         #:width 500))

(time
 (plot2d (list (function sin #:label "Sine"
                         #:color "Blue" #:style 'long-dash #:width 3)
               (function sqr #:label "Square"))
               #:x-min -3 #:x-max 3
               #:legend-anchor 'top-left))

(time
 (plot2d (list (axes)
               (function sqr -2.1 2.1 #:label "x^2")
               (error-bars (map (λ (x) (vector x (sqr x) (/ 1/2 (+ (abs x) 1))))
                                (sequence->list (in-range -2 2.1 1/8)))
                           #:x-min -1))))

(time
 (plot2d (list (points '(#(1 1) #(2 2) #(3 3)) #:symbol "bob" #:size 10
                       #:x-min 2 #:x-max 3 #:y-min 1 #:y-max 3))
         #:x-min 0 #:x-max 4 #:y-min 0 #:y-max 4))

(time
 (plot2d (list (x-axis 1 #:ticks? #t) (y-axis 1 #:ticks? #t)
               (function sqr #f 2 #:color 1)
               (inverse sqr #:color 2)
               (function values #:color 0 #:style 1))
         #:x-min -2 #:y-min -1))

(time
 (plot2d (list (polar-axes #:number 4)
               (polar (λ (θ) (+ 0.5 (cos (* 1/2 θ)))) (* -2 pi) (* 2 pi)))))

; draws both functions with x in [-1,1]
(plot2d (list (function sqr #f 1)
              (function (λ (x) (* 2 (sqr x))) -1 #f
                        #:color "blue")
              (axes 1 0 #:x-ticks? #t #:y-ticks? #t)))

#;; error: could not determine x bounds
(plot2d (list (function sqr #f -1)
              (function sqr 1 #f)))

; draws first function with x in [-2,-1]
(plot2d (list (function sqr #f -1)
              (function sqr 1 #f))
        #:x-min -2)

; draws second function with x in [1,2]
(plot2d (list (function sqr #f -1)
              (function sqr 1 #f))
        #:x-max 2)

; draws both functions with x in [-2,2]
(plot2d (list (function sqr #f -1)
              (function sqr 1 #f))
        #:x-min -2 #:x-max 2)

(time
 (plot2d (list (histogram (build-list 10 (λ (n)
                                           (vector (string-ref "abcdefghij" n)
                                                   (sqr n))))
                          #:label "ord(x)^2")
               (function truncate))))

(time
 (plot2d (list (x-axis)
               (histogram '(#(a -1) #(b 2.6) #(c 4) #(d 3.1))
                          #:color 5 #:line-color 5 #:line-style 'long-dash
                          #:label "Corrupt")
               (histogram '(#(a 1) #(b 2.6) #(c 4) #(d 3.1))
                          0 #:x-min 5
                          #:color 1 #:line-color 1 #:line-width 3
                          #:label "Clean"))
         #:title "Widgetyness of Widgets"
         #:x-label "Widget"
         #:y-label "Widgetyness"
         #:legend-anchor 'bottom-right))

(time
 (define xs (build-list 10000 (λ _ (random))))
 (define ys (build-list 10000 (λ _ (random))))
 (plot2d (list
          (points (map vector xs ys)
                  #:x-min -1 #:x-max 1 #:y-min 0.5 #:y-max 1
                  #:symbol 'fullcircle #:size 6.5 #:alpha 1/8
                  #:label "Dots"))
         #:y-max 1.5))

(time
 (plot2d (vector-field vector -0.5 1.85 -5 0.5
                       #:color "blue" #:line-width 2/3
                       #:arrow-scale 'auto #:label "(vector x y)")
         #:x-min -1 #:x-max 5))

(time
 (plot2d (list (function (λ (x) (* 220 (cos (* 4 x)))) -2 2)
               (function (λ (x) (* 200 (sin (* 3 x)))) 0 #f
                         #:y-min -150 #:y-max 150
                         #:color "blue"))
         #:x-min -1/2 #:x-max 3))

(time
 (plot2d (lines (reverse
                 (for/fold ([lst (list (vector 0 0))]) ([i  (in-range 1 400)])
                   (match-define (vector x y) (first lst))
                   (cons (vector i (+ y (* 1/100 (- (random) 1/2)))) lst)))
                #:alpha 0.5 #:label "Random walk")))

(time
 (plot2d (function (λ (x) (/ 1.0 (exact->inexact x))) -2 2)))

(time
 (plot2d (parametric (λ (t) (vector (sin t) (cos t))) (- pi) pi
                     #:x-min -0.5 #:y-max 0.5)
         #:x-min -1))

(time
 (plot2d (list (function sin -1/2 1)
               (parametric (λ (t) (vector (cos t) (sin t))) -2 1
                           #:color "blue" #:style 'short-dash))))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (+ (norm -1.5 -1.5 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))))

(define (f2 x y)
  (- (sqr x) (sqr y)))

(time (plot2d (list (contours f1 0 5 #:label "Cyan/Redness")
                    (contours f2 -5 0 #:colors '("blue") #:label "Blueness"
                              #:widths '(2) #:styles '(dot)))
              #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

(time (plot2d (contour-intervals f1 -5 5 -5 5 #:label "z")))

(time (plot2d (list (tick-grid)
                    (contour-intervals f1 -5 2 -5 2
                                       #:contour-styles '(transparent)
                                       #:label "")
                    (contours f1 -2 5 -2 5 #:label ""))
              #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5
              #:legend-anchor 'center))

(time (plot2d (list (tick-grid)
                    (contour-intervals f1 -5 2 -5 2
                                       #:colors default-contour-line-colors
                                       #:styles '(0 1 2 3 4 5 6)
                                       #:contour-styles '(transparent)
                                       #:label "z")
                    (contours f1 -2 5 -2 5))
              #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5
              #:legend-anchor 'top-left))

; tests contour 7-sided and 8-sided saddle facets
; contour shading should line up with contour lines, no matter how weird
(parameterize ([contour-samples  10])
  (define (f x y) (sqr (sin (- x y))))
  (time (plot2d (contour-intervals f)
                #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
               [plot2d-x-label "Torsion"]
               [plot2d-y-label "Widgetyness"])
  (time
   (plot2d->file (contour-intervals f1 #:alphas '(0.5))
                 "contour-test.png"
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

  (time
   (plot2d->file (contour-intervals f1)
                 "contour-test.ps"
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

  (time
   (plot2d->file (contour-intervals f1 #:alphas '(0.5))
                 "contour-test.pdf"
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

  (time
   (plot2d->file (contour-intervals f1)
                 "contour-test.svg"
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(time
 (define (f2 x) (sin (* x pi)))
 (plot2d (list (x-tick-lines)
               (function-interval atan (λ (x) (- (atan x)))
                                   #:color 6 #:line1-color 6 #:line2-color 6)
               (function-interval sqr f2 -1 1 #:alpha 0.5)
               (function-label f2 -1/4 #:anchor 'top-left))))

(time
 (define amps (linear-seq 1/4 1 8))
 (define colors (color-seq* '("darkred" "white" "darkblue") 7))
 (plot2d (flatten
          (list
           (x-tick-lines)
           (for/list ([a1     (in-list amps)]
                      [a2     (in-list (rest amps))]
                      [color  (in-list colors)])
             (inverse-interval (λ (y) (* a1 (sin y)))
                               (λ (y) (* a2 (sin y)))
                               (- pi) pi #:color color #:alpha 1
                               #:label (format "f(~a,~a)" a1 a2)))
           (y-tick-lines)
           (inverse-label (λ (y) (* 1/4 (sin y))) (* -1/2 pi)
                          "x = 1/4 sin(y)")))
         #:legend-anchor 'top-left))

(time
 (define a #(0 0))
 (define b #(1 1/2))
 (define c #(0 1))
 (define d #(1 3/2))
 (plot2d (list
          (tick-grid)
          (lines-interval (list a b) (list c d)
                          #:color 4 #:line1-color 4 #:line2-color 4
                          #:label "Parallelogram")
          (point-label #(1/2 5/4) #:anchor 'bottom-right #:alpha 0.5))
         #:legend-anchor 'bottom-left))

(time
 (define (fa t) (vector (* 2 (cos (* 4/5 t))) (* 2 (sin (* 4/5 t)))))
 (define (fb t) (vector (cos t) (sin t)))
 (define (fc t) (vector (* 1/2 (cos (* 4/5 t))) (* 1/2 (sin (* 4/5 t)))))
 (define t1 (- pi))
 (define t2 pi)
 (plot2d (list
          (x-tick-lines)
          (lines (list (fa t1) (fb t1) (vector +nan.0 +nan.0) (fb t1) (fc t1))
                 #:color "black" #:style 'dot)
          (lines (list (fa t2) (fb t2) (vector +nan.0 +nan.0) (fb t2) (fc t2))
                 #:color "black" #:style 'dot)
          (parametric fa t1 t2 #:color 5 #:label "fa")
          (parametric-interval fa fb t1 t2 #:color 5 #:label "(fa,fb)"
                               #:line1-style 'transparent
                               #:line2-style 'transparent)
          (parametric fb t1 t2 #:color 1 #:label "fb")
          (parametric-interval fb fc t1 t2 #:color 2 #:label "(fb,fc)"
                               #:line1-style 'transparent
                               #:line2-style 'transparent)
          (parametric fc t1 t2 #:color 2 #:label "fc")
          (x-axis #:ticks? #t)
          (parametric-label fa t1 "fa(-π)"
                            #:size 14 #:anchor 'left #:point-size 5)
          (parametric-label fa t2 "fa(π)"
                            #:size 14 #:anchor 'left #:point-size 5))
         #:legend-anchor 'top-right))

(time
 (define (f1 θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))
 (define (f2 θ) (+ 1 (* 1/4 (cos (* 10 θ)))))
 (plot2d (list (polar-axes #:number 10)
               (polar-interval f1 f2
                               #:color 3 #:label "[f1,f2]"
                               #:line1-color 1 #:line1-width 2 #:line1-style 'dot
                               #:line2-color 2 #:line2-width 2)
               (polar-label f1 0 #:anchor 'top-left)
               (polar-label f2 (degrees->radians 36) #:anchor 'bottom-right)
               (point-label #(1/2 1/2)))))

(time
 (define (f1 θ) (/ θ pi 2))
 (define (f2 θ) (+ (/ θ pi 2) 1))
 (plot2d (list (tick-grid)
               (polar-interval f1 f2 0 (* 5 pi)
                               #:color 4 #:alpha 3/4
                               #:line1-color 1 #:line2-color 1
                               #:label "[f1,f2]"))
         #:legend-anchor 'center))

(time
 (define ((make-fun y) x)
   (+ y (sqr x)))
 
 (values
  (plot2d (build-list
           20 (λ (n) (function (make-fun n) #:color n #:style n #:width 2)))
          #:x-min -2 #:x-max 2)
  
  (plot2d (flatten
           (list
            (tick-grid)
            (function-interval (λ (x) 0) (λ (x) 16) #:color "black" #:alpha 1/20)
            (build-list
             12 (λ (n) (function-interval
                        (make-fun n) (make-fun (add1 n)) -2 0
                        #:color (->color/line n) #:style n #:alpha 1
                        #:line1-style 'transparent #:line2-style 'transparent)))
            (build-list
             12 (λ (n) (function-interval
                        (make-fun n) (make-fun (add1 n)) 0 2
                        #:color n
                        #:line1-style 'transparent #:line2-style 'transparent)))
            (build-list
             13 (λ (n) (function (make-fun n) -2 0
                                 #:color n #:width 2)))
            (build-list
             13 (λ (n) (function (make-fun n) 0 2
                                 #:color n #:width 2 #:style n)))))
          #:x-min -2 #:x-max 2)))
