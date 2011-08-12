#lang racket/base

(require racket/math racket/flonum)

(provide (all-defined-out))

(define (real-seq start stop num)
  (define size (- stop start))
  (define step (/ size (sub1 num)))
  (build-list num (λ (n) (+ start (* n step)))))

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

(define (nan? x) (eqv? x +nan.0))
(define (inf? x) (or (eqv? x +inf.0) (eqv? x -inf.0)))
(define (reg? x) (and (not (nan? x)) (not (inf? x))))

(define (reg-min* xs)
  (apply min (filter reg? xs)))

(define (reg-max* xs)
  (apply max (filter reg? xs)))

(define 180/pi (fl/ 180.0 pi))
(define pi/180 (fl/ pi 180.0))

(define (degrees->radians d)
  (fl* (exact->inexact d) pi/180))

(define (radians->degrees r)
  (fl* (exact->inexact r) 180/pi))

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
