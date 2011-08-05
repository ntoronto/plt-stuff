#lang racket/base

(require racket/math racket/match racket/vector racket/gui racket/flonum)

(provide (all-defined-out))

;; =============================================================================
;; Color junk

(define (color->color% c)
  (match-define (list r g b) c)
  (make-object color% r g b))

(define (float->color-byte f)
  (define i (inexact->exact (round f)))
  (if (i . < . 0) 0 (if (i . > . 255) 255 i)))

;; =============================================================================
;; Missing/simple math functions

;; atan2 : real real -> real
;; Like atan, but accepts rise and run instead of slope so it can return values
;; in the range [-pi,pi].
(define (atan2 y x)
  (angle (make-rectangular x y)))

;; flatan2 : flonum flonum -> flonum
(define (flatan2 y x)
  (cond [(x . fl> . 0.0)  (flatan (fl/ y x))]
        [(x . fl< . 0.0)  (cond [(y . fl< . 0.0)  (fl- (flatan (fl/ y x)) pi)]
                                [else             (fl+ (flatan (fl/ y x)) pi)])]
        [else             (cond [(y . fl< . 0.0)  (fl* pi -0.5)]
                                [(y . fl> . 0.0)  (fl* pi  0.5)]
                                [else             0.0])]))

(define (maybe-min* x y)
  (if x (if y (min x y) x)
      (if y y #f)))

(define (maybe-min . xs)
  (foldl maybe-min* #f xs))

(define (maybe-max* x y)
  (if x (if y (max x y) x)
      (if y y #f)))

(define (maybe-max . xs)
  (foldl maybe-max* #f xs))

(define (floor-log10 x)
  (inexact->exact (floor (/ (log x) (log 10)))))

(define (ceiling-log10 x)
  (inexact->exact (ceiling (/ (log (abs x)) (log 10)))))

;; Like real->decimal-string, but removes trailing zeros
(define (real->string/trunc x e)
  (define str (real->decimal-string x e))
  (let loop ([x  (string-length str)])
    (cond [(zero? x)  "0"]
          [(char=? #\0 (string-ref str (sub1 x)))  (loop (sub1 x))]
          [(char=? #\. (string-ref str (sub1 x)))  (substring str 0 (sub1 x))]
          [else  (substring str 0 x)])))

(define (real-seq start stop num)
  (define size (- stop start))
  (define step (/ size (sub1 num)))
  (build-list num (λ (n) (+ start (* n step)))))

(define (sample-2d-function f x-min x-max y-min y-max samples)
  (define xs (real-seq (exact->inexact x-min) (exact->inexact x-max) samples))
  (define ys (real-seq (exact->inexact y-min) (exact->inexact y-max) samples))
  
  ; cache every (f x y) in a vector of flvectors
  (define zss
    (for/vector #:length samples ([y  (in-list ys)])
      (for/flvector #:length samples ([x  (in-list xs)])
        (exact->inexact (f x y)))))
  
  (define-values (z-min z-max)
    (for*/fold ([z-min +inf.0] [z-max -inf.0]) ([zs  (in-vector zss)]
                                                [z   (in-flvector zs)])
        (values (flmin z z-min) (flmax z z-max))))
  
  (values xs ys zss z-min z-max))

;; =============================================================================
;; Small vector library

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
