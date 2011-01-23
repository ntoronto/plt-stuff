#lang typed/racket/base

(require "private/utils.rkt" "private/bf.rkt" "private/syntax.rkt"
         "private/simple.rkt" "private/arithmetic.rkt"
         "private/error.rkt" "private/rounding.rkt"
         "private/bfagm.rkt" "private/bf-pi.rkt" "private/bflog.rkt"
         "private/bfexp.rkt" "private/exponential.rkt"
         "private/trigonometric.rkt" "private/inverse-trigonometric.rkt"
         "private/hyperbolic.rkt" "private/inverse-hyperbolic.rkt"
         "private/bfrandom.rkt" "private/statistical.rkt")

;; Provide everything except utils.rkt, syntax.rkt, and a couple of boxes
(provide (except-out
          (all-from-out
           "private/bf.rkt"
           "private/simple.rkt" "private/arithmetic.rkt"
           "private/error.rkt" "private/rounding.rkt"
           "private/bfagm.rkt" "private/bf-pi.rkt" "private/bflog.rkt"
           "private/bfexp.rkt" "private/exponential.rkt"
           "private/trigonometric.rkt" "private/inverse-trigonometric.rkt"
           "private/hyperbolic.rkt" "private/inverse-hyperbolic.rkt"
           "private/bfrandom.rkt" "private/statistical.rkt")
          big-decimal-float->bigfloat-box
          bigfloat->big-decimal-float-box))

#|  What's going on with the boxes?

There's a minor circular dependency involving bf.rkt, bflog.rkt, and bfexp.rkt.
Moving the necessary code into bf.rkt to resolve it would make bf.rkt very large
and apparently badly factored.

The problem is that `bigfloat->string' and `string->bigfloat' need to call
`bfexpt' and `bflogb' to convert numbers with large exponents. (Floating-point
`exp' and `log' aren't precise enough when exponents get large.) So bf.rkt has
two boxes that hold either `#f' or a procedure that does the hard parts. The
string conversion procedures check the box when exponents are large, and call
the procedures when they're "installed".

The following are the hard-part functions, and a couple of `set-box!'s that
install them.

(I know the "obvious" solution is to use units, but I don't like units, and I
would be using them only to resolve a small issue that I can work around while
developing modules. The mental/emotional/spritual/karmic/maintainence overhead
seems not to be worth it.)
|#

;; =============================================================================
;; bigfloat to decimal

(: bigfloat->big-decimal-float (bigfloat -> (values Fixnum Integer Integer)))
(define (bigfloat->big-decimal-float x)
  (def (bigfloat x-sig ax-exp) x)
  (def x-bits (max 2 (integer-bits x-sig)))
  (parameterize ([bf-bits  (+ 10 (max 16 x-bits) (ceiling-log2i x-bits)
                              ; need enough bits for ay-exp to be exact:
                              (integer-bits ax-exp))])
    (def sgn (if (negative? x-sig) -1 1))
    (def ax-sig (abs x-sig))
    (def ay-exp
      (bfsub1 (bftruncate (bf* (bf ax-exp) (bf/ (bf-log-2) (bf-log-10))))))
    (def ay-sig (bfround (bf/ (bigfloat ax-sig ax-exp) (bfexp10 ay-exp))))
    (values sgn (bigfloat->integer ay-sig) (bigfloat->integer ay-exp))))

(set-box! bigfloat->big-decimal-float-box bigfloat->big-decimal-float)

;; =============================================================================
;; decimal to bigfloat

(: big-decimal-float->bigfloat (Fixnum Natural Integer -> bigfloat))
(define (big-decimal-float->bigfloat sgn ay-sig ay-exp)
  (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits))
                   ; need enough bits for ax-exp to be exact:
                   (+ 4 (integer-bits ay-exp)))
    (def ax-exp
      (bigfloat->integer
       (bfsub1 (bftruncate (bf* (bf ay-exp) (bf/ (bf-log-10) (bf-log-2)))))))
    (def ax-sig
      (bigfloat->integer (bfround (bf* (bigfloat ay-sig (- ax-exp))
                                       (bfexp10 (bf ay-exp))))))
    (bigfloat (* sgn ax-sig) ax-exp)))

(set-box! big-decimal-float->bigfloat-box big-decimal-float->bigfloat)
