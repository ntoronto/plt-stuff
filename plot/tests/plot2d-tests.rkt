#lang racket

(require "../plot2d.rkt")

(time
 (plot2d (histogram (build-list 10 (λ (n) (cons n (sqr n)))))))

(time
 (plot2d (mix (histogram '((a . -1) (b . 2.6) (c . 4) (d . 3.1))
                         #:bar-color "blue"
                         #:line-color "white" #:line-style 'long-dash)
              (histogram '((a . 1) (b . 2.6) (c . 4) (d . 3.1))
                         0 #:x-min 2.1 #:x-max 3.1
                         #:bar-color "red"
                         #:line-color "black" #:line-width 3))
         #:title "Widgetyness of Widgets"
         #:x-label "Widget"
         #:y-label "Widgetyness"))

(define xs (build-list 10000 (λ _ (random))))
(define ys (build-list 10000 (λ _ (random))))
(time
 (plot2d (points (map vector xs ys) -1 1 0.5 1
                 #:color "black" #:label 'dot #:size 6 #:alpha 0.5)
         #:y-max 1.5))

(time
 (plot2d (vector-field values -0.5 1.85 #f 0.5
                       #:color "blue" #:line-width 2/3
                       #:arrow-length 'scaled)
         #:x-min -1 #:x-max 5))

(time
 (plot2d (mix (function (λ (x) (* 220 (cos (* 4 x)))) -2 2)
              (function (λ (x) (* 200 (sin (* 3 x)))) 0 #f -190 190
                        #:color "blue"))
         #:x-min -1/2 #:x-max 3))

(time
 (plot2d (lines (map vector (build-list 1000 values) (take xs 1000))
                #:alpha 0.5)))

(time
 (plot2d (function (λ (x) (/ 1.0 (exact->inexact x))) -2 2)))

(time
 (plot2d (parametric (vector sin cos) (- pi) pi -0.5 #f #f 0.5)
         #:x-min -1))

(time
 (plot2d (mix (function sin -1/2 1)
              (parametric (vector cos sin) -2 1
                          #:color "blue" #:style 'short-dash))))

; tests contour 7-sided and 8-sided saddle facets
; contour shading should line up with contour lines, no matter how weird
(parameterize ([contour-samples  10])
  (define (f x y) (sqr (sin (- x y))))
  (time (plot2d (mix (shade f) (contour f)))))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (+ (norm -1.5 -1.5 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))))

(define (f2 x y)
  (- (sqr x) (sqr y)))

(time (plot2d (mix (contour f1 0 5)
                   (contour f2 -5 0 #:color "blue" #:width 2 #:style 'dot))))

(time (plot2d (mix (shade f1 -5 2 -5 2)
                   (contour f1 -2 5 -2 5))))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (contour f1) "contour-test.png" 'png)))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (contour f1) "contour-test.ps" 'ps)))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (mix (shade f1) (contour f1))
                 "contour-test.pdf" 'pdf)))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (contour f1) "contour-test.svg" 'svg)))
