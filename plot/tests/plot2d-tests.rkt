#lang racket

(require "../plot2d.rkt")

;(plot2d-new-window? #t)

(plot2d (list (function sqr -2.1 2.1)
              (error-bars (map (λ (x) (vector x (sqr x) (/ 1/2 (+ (abs x) 1))))
                               (sequence->list (in-range -2 2.1 1/8))))))

(plot2d (list (points '(#(1 1) #(2 2) #(3 3)) #:label "bob" #:size 10
                      #:x-min 2 #:x-max 3 #:y-min 1 #:y-max 3))
        #:x-min 0 #:x-max 4 #:y-min 0 #:y-max 4)

(plot2d (list (function sqr #f 2)
              (inverse sqr))
        #:x-min -2 #:y-min -1)

; draws both functions with x in [-1,1]
(plot2d (list (function sqr #f 1)
              (function (λ (x) (* 2 (sqr x))) -1 #f
                        #:color "blue")))

#;; error: could not determine sensible x bounds
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
                                                   (sqr n)))))
               (function truncate))))

(time
 (plot2d (list (histogram '(#(a -1) #(b 2.6) #(c 4) #(d 3.1))
                          #:bar-color "blue"
                          #:line-color "white" #:line-style 'long-dash)
               (histogram '(#(a 1) #(b 2.6) #(c 4) #(d 3.1))
                          0 #:x-min 5
                          #:bar-color "red"
                          #:line-color "black" #:line-width 3))
         #:title "Widgetyness of Widgets"
         #:x-label "Widget"
         #:y-label "Widgetyness"))

(define xs (build-list 10000 (λ _ (random))))
(define ys (build-list 10000 (λ _ (random))))
(time
 (plot2d (points (map vector xs ys)
                 #:x-min -1 #:x-max 1 #:y-min 0.5 #:y-max 1
                 #:color "black" #:label 'dot #:size 6 #:alpha 0.5)
         #:y-max 1.5))

(time
 (plot2d (vector-field vector -0.5 1.85 -5 0.5
                       #:color "blue" #:line-width 2/3
                       #:arrow-length 'scaled)
         #:x-min -1 #:x-max 5))

(time
 (plot2d (list (function (λ (x) (* 220 (cos (* 4 x)))) -2 2)
               (function (λ (x) (* 200 (sin (* 3 x)))) 0 #f
                         #:y-min -150 #:y-max 150
                         #:color "blue"))
         #:x-min -1/2 #:x-max 3))

(time
 (plot2d (lines (map vector (build-list 1000 values) (take xs 1000))
                #:alpha 0.5)))

(time
 (plot2d (function (λ (x) (/ 1.0 (exact->inexact x))) -2 2)))

(time
 (plot2d (parametric (vector sin cos) (- pi) pi
                     #:x-min -0.5 #:y-max 0.5)
         #:x-min -1))

(time
 (plot2d (list (function sin -1/2 1)
               (parametric (vector cos sin) -2 1
                           #:color "blue" #:style 'short-dash))))

; tests contour 7-sided and 8-sided saddle facets
; contour shading should line up with contour lines, no matter how weird
(parameterize ([contour-samples  10])
  (define (f x y) (sqr (sin (- x y))))
  (time (plot2d (list (shade f) (contour f))
                #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (+ (norm -1.5 -1.5 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))))

(define (f2 x y)
  (- (sqr x) (sqr y)))

(time (plot2d (list (contour f1 0 5)
                    (contour f2 -5 0 #:color "blue" #:width 2 #:style 'dot))
              #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

(time (plot2d (list (shade f1 -5 2 -5 2)
                    (contour f1 -2 5 -2 5))
              #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (contour f1) "contour-test.png" 'png
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (contour f1) "contour-test.ps" 'ps
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (list (shade f1) (contour f1))
                 "contour-test.pdf" 'pdf
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(time
 (parameterize ([plot2d-title  "Survival Rate of Torsion Widgets"]
                [plot2d-x-label "Torsion"]
                [plot2d-y-label "Widgetyness"])
   (plot2d->file (contour f1) "contour-test.svg" 'svg
                 #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))
