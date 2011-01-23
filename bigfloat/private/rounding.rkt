#lang typed/racket/base

(require "utils.rkt" "bf.rkt")

(provide (all-defined-out))

(: bfround (bigfloat -> bigfloat))
(define (bfround x)
  (def (bigfloat x-sig x-exp) x)
  (cond [(negative? x-exp)
         (def new-bits (+ (integer-bits x-sig) x-exp))
         (def shift (- (bf-bits) new-bits))
         (cond [(positive? shift)
                (def (values new-sig adj)
                  (rounding-shift x-sig x-exp))
                (if (zero? adj)
                    (bigfloat (new-sig . << . shift) (- shift))
                    (bigfloat (new-sig . << . (+ shift adj))
                              (- (- shift) adj)))]
               [(negative? shift)
                (def (values new-sig adj)
                  (rounding-shift x-sig (+ x-exp shift)))
                (if (zero? adj)
                    (bigfloat new-sig (- shift))
                    (bigfloat (new-sig . << . adj) (- (- shift) adj)))]
               [else
                (def (values new-sig adj)
                  (rounding-shift x-sig x-exp))
                (if (zero? adj)
                    (bigfloat new-sig 0)
                    (bigfloat (new-sig . << . adj) (- adj)))])]
        [else  (new-bigfloat x-sig x-exp)]))

(: bffloor (bigfloat -> bigfloat))
(define (bffloor x)
  (def (bigfloat x-sig x-exp) x)
  (if (negative? x-exp)
      (new-bigfloat (x-sig . << . x-exp) 0)
      (new-bigfloat x-sig x-exp)))

(: bftruncate (bigfloat -> bigfloat))
(define (bftruncate x)
  (def (bigfloat x-sig x-exp) x)
  (cond [(negative? x-exp)
         (def y-sig (x-sig . << . x-exp))
         (if (and (negative? x-sig)
                  (not (zero? (bitwise-low-bits x-sig (- x-exp)))))
             (new-bigfloat (add1 y-sig) 0)
             (new-bigfloat y-sig 0))]
        [else  (new-bigfloat x-sig x-exp)]))

(: bfceiling (bigfloat -> bigfloat))
(define (bfceiling x)
  (def (bigfloat x-sig x-exp) x)
  (cond [(negative? x-exp)
         (def y-sig (x-sig . << . x-exp))
         (if (zero? (bitwise-low-bits x-sig (- x-exp)))
             (new-bigfloat y-sig 0)
             (new-bigfloat (add1 y-sig) 0))]
        [else  (new-bigfloat x-sig x-exp)]))
