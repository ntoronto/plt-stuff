#lang racket

(require "../plot3d.rkt")

;(plot3d-new-window? #t)

(time
 (plot3d (surface3d (λ (x y) (+ (/ x) (/ y))) -2 2 -2 2
                    #:color '(255 128 128)
                    #:line-color '(255 128 128)
                    #:line-width 1.5)
         #:title "Here it is!"
         #:x-label "WannaHockaLoogi"
         #:y-label "An Impossibly Long Y Axis Label"
         #:angle 330 #:altitude 0))

(time
 (plot3d (surface3d (λ (x y)
                      (+ (/ (+ (abs x) 0.01))
                         (/ (+ (abs y) 0.01))))
                    -4 4 -4 4 #:color '(128 128 255))
         #:angle 330 #:altitude 41
         #:z-label #f #:y-label #f #:x-label #f
         ))

(let ()
  (define xs (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (define ys (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (define zs (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (time
   (plot3d (points3d (map vector xs ys zs)
                     #:x-min -20 #:x-max 20
                     #:y-min -20 #:y-max 20
                     #:z-min -20 #:z-max 20)
           #:angle 15 #:title "Random Points")))

(let ()
  (define xs (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (define ys (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (define zs (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (time
   (plot3d (points3d (map vector xs ys zs)
                     #:x-min -20 #:x-max 20
                     #:y-min -20 #:y-max 20
                     #:z-min -20 #:z-max 20
                     #:color "blue" #:label 'dot ;#:size 10
                     #:alpha 0.5)
           #:angle 30 #:altitude 30
           #:title "A Bunch of Random Points Concentrated at the Origin"
           #:x-label "x" #:y-label "y" #:z-label "z")))

;; tests line clipping: should look like a sphere with six poles chopped off
(parameterize ([line3d-samples 3000])
  (time
   (plot3d (parametric3d (λ (t)
                           (vector (* (cos (* 80 t)) (cos t))
                                   (* (sin (* 80 t)) (cos t))
                                   (sin t)))
                         (- pi) pi
                         #:x-min -0.8 #:x-max 0.8
                         #:y-min -0.8 #:y-max 0.8
                         #:z-min -0.8 #:z-max 0.8
                         #:color "blue" #:width 1/2 #:style 'long-dash
                         #:alpha 0.5)
           #:altitude 22
           #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1)))

(time
 (plot3d (surface3d (λ (x y) (+ x y)) -0.81 0.81 -0.81 0.81
                    #:line-color '(0 0 255) #:line-width 1 #:line-style 'dot)
         #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (- (sqr x) (sqr y)))

(define (f2 x y)
  (- (sqrt (+ (abs y) (abs x)))))

(define (f3 x y)
  (define d (* 2 pi (+ (abs x) (abs y))))
  (+ (* 1/8 (cos d)) (- (sqr x) (sqr y))))

(define (f4 x y)
  (imag-part (log (make-rectangular x y))))

(define (f5 x y)
  (+ (* 1.1 (norm -1.5 -1.5 x y))
     (* 2 (norm 1 1 x y))
     (* 1.3 (norm 2 -2 x y))))

(define (f6 x y)
  (define d (sqrt (+ (sqr x) (sqr y))))
  (if (d . < . 1)
      (sqrt (- 1 (sqr d)))
      0))

(time
 (plot3d (list (surface3d f5 0 4 -4 4 #:color '(128 255 160) #:alpha 0.5)
               (shade3d f5 -4 0 -4 4
                        #:line-colors (λ _ '((128 0 128))) #:line-width 1
                        #:line-style 'long-dash #:alpha 0.75))
         #:z-min 0.25 #:z-max 1.1))

(time
 (plot3d (list (contour3d f5 -4 4 -4 4 #:line-style 'long-dash)
               (shade3d f5 -2.5 2.5 -2.5 2.5
                        #:z-min 0.25 #:z-max 1.5))))

(time
 (plot3d (shade3d f5 -3 3 -3 3
                  #:colors (λ _ '((255 128 128) (128 128 255))))))

(time
 (plot3d (list (surface3d f4 -4 4 -4 4 #:color '(255 224 0))
               (contour3d f4 -4 4 -4 4))
         #:angle -30))

(time
 (plot3d->file (shade3d f5 -5 5 -5 5 #:alpha 0.75)
               "contour3d-test.pdf" 'pdf))

(time
 (plot3d->file (shade3d f5 -5 5 -5 5 #:alpha 0.75)
               "contour3d-test.ps" 'ps))

(time
 (plot3d->file (shade3d f5 -5 5 -5 5 #:alpha 0.75)
               "contour3d-test.svg" 'svg))

(time
 (plot3d->file (shade3d f5 -5 5 -5 5 #:alpha 0.75)
               "contour3d-test.png" 'png))

(time
 (plot3d (list (shade3d f5 -4 4 -4 4)
               (contour3d f5 -4 4 -4 4 #:line-color '(128 0 128)))))

(time
 (plot3d (list (shade3d f1 -4 4 -4 4)
               (contour3d f1 -4 4 -4 4))))
