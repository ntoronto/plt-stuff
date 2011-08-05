#lang racket/base

(require "utils.rkt")

(provide (all-defined-out))

(define (range->ticks x-min x-max)
  (let ([x-min  (inexact->exact x-min)]
        [x-max  (inexact->exact x-max)])
    (define e (floor-log10 (- x-max x-min)))
    (define mag (expt 10 e))
    (define step
      (let ([y  (/ (- x-max x-min) mag)])
        (cond [(y . < . 2)   (* 1/5 mag)]
              [(y . < . 5)   (* 1/2 mag)]
              [(y . < . 10)  mag])))
    (define start (* (ceiling (/ x-min step)) step))
    (define stop (* (floor (/ x-max step)) step))
    (define num (+ 1 (round (/ (- stop start) step))))
    (real-seq start stop num)))

(define (format-tick-label x)
  (cond [(zero? x)  "0"]
        [else
         (define e (ceiling-log10 x))
         (cond [(e . > . 4)
                (format "~ae~a" (real->string/trunc (/ x (expt 10 e)) 15) e)]
               [(e . < . -4)
                (format "~ae~a" (real->string/trunc (/ x (expt 10 e)) 15) e)]
               [else            (real->string/trunc x 15)])]))

(define (draw-ticks mn mx tick-major draw-tick)
  (define ticks (range->ticks mn mx))
  (cond [(member 0 ticks)
         (draw-tick 0 #t)
         (for ([x  (in-list (filter (λ (x) (x . > . 0)) ticks))]
               [n  (in-naturals)])
           (draw-tick x (zero? (remainder (add1 n) tick-major))))
         (for ([x  (in-list (reverse (filter (λ (x) (x . < . 0)) ticks)))]
               [n  (in-naturals)])
           (draw-tick x (zero? (remainder (add1 n) tick-major))))]
        [else
         (for ([x  (in-list ticks)] [n  (in-naturals)])
           (draw-tick x (zero? (remainder n tick-major))))]))
