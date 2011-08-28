#lang racket/base

(require racket/match racket/vector racket/math
         "math.rkt")

(provide (all-defined-out))

(define (vcross v1 v2)
  (match-define (vector x1 y1 z1) v1)
  (match-define (vector x2 y2 z2) v2)
  (vector (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2))))

(define (vcross2 v1 v2)
  (match-define (vector x1 y1) v1)
  (match-define (vector x2 y2) v2)
  (- (* x1 y2) (* y1 x2)))

(define (v+ v1 v2) (vector-map + v1 v2))
(define (v- v1 v2) (vector-map - v1 v2))
(define (vneg v) (vector-map - v))
(define (v* v c) (vector-map (λ (x) (* x c)) v))
(define (v/ v c) (vector-map (λ (x) (/ x c)) v))

(define (vmag^2 v) (apply + (vector->list (vector-map sqr v))))
(define (vdot v1 v2) (apply + (vector->list (vector-map * v1 v2))))
(define (vmag v) (sqrt (vmag^2 v)))
(define (vnormalize v) (v/ v (vmag v)))

(define (vall-regular? v)
  (let/ec return
    (for ([x  (in-vector v)])
      (when (not (regular? x))
        (return #f)))
    #t))

(define (vregular-sublists vs)
  (cond [(null? vs)  (list null)]
        [(vall-regular? (car vs))  (define rst (vregular-sublists (cdr vs)))
                                   (cons (cons (car vs) (car rst)) (cdr rst))]
        [else  (cons null (vregular-sublists (cdr vs)))]))

(define (bounding-box vs)
  (match-define (list (vector xs ys zs) ...) vs)
  (values (apply min xs) (apply max xs)
          (apply min ys) (apply max ys)
          (apply min zs) (apply max zs)))

;; returns the center of the smallest axial bounding rectangle containing the
;; points
(define (center-coord vs)
  (define-values (x-min x-max y-min y-max z-min z-max)
    (bounding-box vs))
  (vector (* 1/2 (+ x-min x-max))
          (* 1/2 (+ y-min y-max))
          (* 1/2 (+ z-min z-max))))

(define default-normal (vector 0 -1 0))

(define (surface-normal vs)
  (define norms
    (for/list ([v1  (in-list vs)]
               [v2  (in-list (append (cdr vs) vs))]
               [v3  (in-list (append (cddr vs) vs))])
      (vcross (v- v2 v1) (v- v3 v1))))
  (define n (foldl v+ (vector 0 0 0) norms))
  (define d^2 (vmag^2 n))
  (if (d^2 . > . 0)
      (v/ n (sqrt d^2))
      default-normal))
