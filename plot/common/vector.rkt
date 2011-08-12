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

(define (vnan? v)
  (let/ec return
    (for ([x  (in-vector v)])
      (when (nan? x)
        (return #t)))
    #f))

(define (vinf? v)
  (let/ec return
    (for ([x  (in-vector v)])
      (when (inf? x)
        (return #t)))
    #f))

(define (vreg? v)
  (let/ec return
    (for ([x  (in-vector v)])
      (when (not (reg? x))
        (return #f)))
    #t))

(define (vreg-sublists vs)
  (cond [(null? vs)  (list null)]
        [(vreg? (car vs))  (define rst (vreg-sublists (cdr vs)))
                           (cons (cons (car vs) (car rst)) (cdr rst))]
        [else  (cons null (vreg-sublists (cdr vs)))]))

(define (center-coord vs)
  (define ((ref n) v) (vector-ref v n))
  (define x-min (apply min (map (ref 0) vs)))
  (define x-max (apply max (map (ref 0) vs)))
  (define y-min (apply min (map (ref 1) vs)))
  (define y-max (apply max (map (ref 1) vs)))
  (define z-min (apply min (map (ref 2) vs)))
  (define z-max (apply max (map (ref 2) vs)))
  (vector (* 1/2 (+ x-min x-max))
          (* 1/2 (+ y-min y-max))
          (* 1/2 (+ z-min z-max))))

(define default-normal (vector 0 -1 0))

(define (surface-normal vs)
  (let/ec return
    (for ([v1  (in-list vs)]
          [v2  (in-list (append (cdr vs) vs))]
          [v3  (in-list (append (cddr vs) vs))])
      (define norm (vcross (v- v2 v1) (v- v3 v1)))
      (define d^2 (vmag^2 norm))
      (when (not (zero? d^2))
        (return (v/ norm (sqrt d^2)))))
    default-normal))
