#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bfexp.rkt")

(provide bfsinh bfcosh bftanh)

(: bfsinh (bigfloat -> bigfloat))
(define (bfsinh x)
  (cond [(bfnegative? x)  (bfneg (bfsinh (bfneg x)))]
        [(bfpositive? x)
         (with-bf-bits (+ (max 16 (bf-bits)) 10 (ceiling-log2i (bf-bits))
                          (max 0 (- (bfceiling-log2 (bfabs x)))))
           (def exp-x (bfexp x))
           (bf/2 (bf- exp-x (bfinv exp-x))))]
        [else  (bf 0)]))

(: bfcosh (bigfloat -> bigfloat))
(define (bfcosh x)
  (with-bf-bits (+ (max 16 (bf-bits)) 10 (ceiling-log2i (bf-bits)))
    (def exp-x (bfexp x))
    (bf/2 (bf+ exp-x (bfinv exp-x)))))

(: bftanh (bigfloat -> bigfloat))
(define (bftanh x)
  (with-bf-bits (+ (max 16 (bf-bits)) 10 (ceiling-log2i (bf-bits))
                   (max 0 (- (bfceiling-log2 x))))
    (def exp-2x (bfexp (bf*2 x)))
    (bf/ (bfsub1 exp-2x) (bfadd1 exp-2x))))
