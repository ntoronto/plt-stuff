#lang racket

(require plot)

(define xs (build-list 10 (λ _ (random))))
(define ys (build-list 10 (λ _ (random))))
(plot (points (map vector xs ys) #:sym 'leftarrow)
      #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1)

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f x y)
  (+ (norm -1.2 -1.2 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))))

(time (plot (contour f)))
(time (plot (shade f)))

(time (plot3d (surface f)
              #:z-min 0 #:z-max 2))

(time (plot (vector-field (λ (xy)
                            (match-define (vector x y) xy)
                            (vector (* 5 x) (* 5 y)))
                          #:style 'scaled)))
