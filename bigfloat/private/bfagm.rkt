#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt")

(require/typed
 racket/base
 [raise-type-error  (Symbol String Integer Any * -> Nothing)]
 [arithmetic-shift  (Natural Integer -> Natural)]
 [integer-sqrt  (Natural -> Natural)])

(provide bfagm)

(: fixagm (Natural Natural -> Natural))
;; Returns the arithmetic-geometric mean of x and y
(define (fixagm x y)
  ;(printf "(fixagm ~v ~v)~n" x y)
  (if (= x y) x (fixagm (arithmetic-shift (+ x y) -1)
                        (integer-sqrt (* x y)))))

(: bfagm (bigfloat bigfloat -> bigfloat))
(define (bfagm x y)
  (cond
    [(bfnegative? x)  (raise-type-error 'bfagm "bigfloat >= 0" 0 x y)]
    [(bfnegative? y)  (raise-type-error 'bfagm "bigfloat >= 0" 1 x y)]
    [(or (bfzero? x) (bfzero? y))  (bf 0)]
    [else
     (with-bf-bits (+ 10 (max 16 (bf-bits))
                      (ceiling-log2i (bf-bits)))
       (def two (bf 2))
       (let loop ([x x] [y y])
         ;(printf "(bfagm ~v ~v)~n" x y)
         ; fixpoint can go a lot faster (it certainly goes no slower)
         (cond
           [((bigfloat-exp x) . = . (bigfloat-exp y))
            (def (bigfloat x-sig x-exp) x)
            (def (bigfloat y-sig y-exp) y)
            (def min-bits (min (integer-length x-sig) (integer-length y-sig)))
            (def shift (- (bf-bits) min-bits))
            (new-bigfloat (fixagm (cast (x-sig . <<r . shift) natural?)
                                  (cast (y-sig . <<r . shift) natural?))
                          (- x-exp shift))]
           [(bf= x y)  x]
           [else  (loop (bf/2 (bf+ x y))
                        (bfsqrt (bf* x y)))])))]))
