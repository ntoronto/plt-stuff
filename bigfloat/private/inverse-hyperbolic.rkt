#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bflog.rkt")

(provide bfasinh bfacosh bfatanh)

(: bfasinh (bigfloat -> bigfloat))
(define (bfasinh x)
  (with-bf-bits (+ (max 16 (bf-bits)) 10 (ceiling-log2i (bf-bits))
                   (max 0 (- (bfceiling-log2 x))))
    (bflog (bf+ (bfsqrt (bfadd1 (bfsqr x))) x))))

(: bfacosh (bigfloat -> bigfloat))
(define (bfacosh x)
  (when (x . bf< . (bf 1))
    (raise-type-error 'bfacosh "bigfloat >= 1" x))
  (with-bf-bits (+ (max 16 (bf-bits)) 10 (ceiling-log2i (bf-bits)))
    (bflog (bf+ (bfsqrt (bfsub1 (bfsqr x))) x))))

(: bfatanh (bigfloat -> bigfloat))
(define (bfatanh x)
  (when ((bfabs x) . bf>= . (bf 1))
    (raise-type-error 'bfatanh "bigfloat in (-1, 1)" x))
  (with-bf-bits (+ (max 16 (bf-bits)) 10 (ceiling-log2i (bf-bits))
                   (max 0 (- (bfceiling-log2 x))))
    (bf/2 (bflog (bf/ (bfadd1 x) (bf- (bf 1) x))))))
