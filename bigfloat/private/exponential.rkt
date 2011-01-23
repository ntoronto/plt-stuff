#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bflog.rkt" "bfexp.rkt")

(require/typed
 racket/base
 [raise-type-error  (Symbol String Integer Any * -> Nothing)])

(provide bfexpt bflogb bfexp2 bfexp10 bflog2 bflog10)

(: bfexpt (bigfloat bigfloat -> bigfloat))
;; Computes exp(x * log(b))
(define (bfexpt b x)
  (cond [(bfnegative? b)  (raise-type-error 'bfexpt "bigfloat >= 0" 0 b x)]
        [(bfzero? x)  (bf 1)]
        [(bfzero? b)  (bf 0)]
        [else  (with-bf-bits (+ 10 (max 16 (bf-bits))
                                (ceiling-log2i (bf-bits))
                                (max (abs (bfceiling-log2 (bfabs b)))
                                     (abs (bfceiling-log2 (bfabs x)))))
                 (bfexp (bf* x (bflog b))))]))

(: bflogb (bigfloat bigfloat -> bigfloat))
(define (bflogb b x)
  (cond [(or (not (bfpositive? b)) (bf= b (bf 1)))
         (raise-type-error 'bflogb "bigfloat > 0 and != 1" 0 b x)]
        [(not (bfpositive? x))  (raise-type-error 'bflogb "bigfloat > 0" 1 b x)]
        [(bf= x (bf 1))  (bf 0)]
        [else
         (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
           (bf/ (bflog x) (bflog b)))]))

(: bfexp2 (bigfloat -> bigfloat))
(define (bfexp2 x) (bfexpt (bf 2) x))

(: bfexp10 (bigfloat -> bigfloat))
(define (bfexp10 x) (bfexpt (bf 10) x))

(: bflog2 (bigfloat -> bigfloat))
(define (bflog2 x) (bflogb (bf 2) x))

(: bflog10 (bigfloat -> bigfloat))
(define (bflog10 x) (bflogb (bf 10) x))
