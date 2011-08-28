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
(define (infinite? x) (or (eqv? x +inf.0) (eqv? x -inf.0)))
(define (regular? x) (and (real? x) (not (nan? x)) (not (infinite? x))))

(define (regular-min . xs)
  (apply min (filter regular? xs)))

(define (regular-max . xs)
  (apply max (filter regular? xs)))

(define 180/pi (fl/ 180.0 pi))
(define pi/180 (fl/ pi 180.0))

(define (degrees->radians d)
  (fl* (exact->inexact d) pi/180))

(define (radians->degrees r)
  (fl* (exact->inexact r) 180/pi))

(define (maybe-min . xs)
  (for/fold ([x  (car xs)]) ([y  (in-list (cdr xs))])
    (if x (if y (min x y) x)
        (if y y #f))))

(define (maybe-max . xs)
  (for/fold ([x  (car xs)]) ([y  (in-list (cdr xs))])
    (if x (if y (max x y) x)
        (if y y #f))))

(define (floor-log10 x)
  (inexact->exact (floor (/ (log (abs x)) (log 10)))))

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

(define (sample-2d-function f x-min x-max x-samples y-min y-max y-samples)
  (define xs (real-seq (exact->inexact x-min) (exact->inexact x-max) x-samples))
  (define ys (real-seq (exact->inexact y-min) (exact->inexact y-max) y-samples))
  (define zss
    (for/vector #:length x-samples ([y  (in-list ys)])
      (for/flvector #:length y-samples ([x  (in-list xs)])
        (exact->inexact (f x y)))))
  (list xs ys zss))

(define (memoize-sample-2d-function f)
  (define memo (make-hash))
  (λ (x-min x-max x-samples y-min y-max y-samples)
    (hash-ref!
     memo (vector x-min x-max x-samples y-min y-max y-samples)
     (λ ()
       (sample-2d-function f x-min x-max x-samples y-min y-max y-samples)))))

(define (2d-sample->list zss)
  (for*/list ([zs  (in-vector zss)]
              [z   (in-flvector zs)])
    z))
