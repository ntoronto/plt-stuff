#lang typed/racket/base

(require (only-in racket/math sqr)
         "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "rounding.rkt")

(require/typed
 racket/base
 [raise-type-error  (Symbol String Integer Any * -> Nothing)]
 [integer-sqrt/remainder (Natural -> (values Natural Natural))])

(provide (except-out (all-defined-out)
                     diffexp+ abs+ diffexp- abs- remainder->rounding-bits abs/))

(: diffexp+ (Natural Natural Natural -> (values Integer Integer)))
; assumes y-exp = 0
(define (diffexp+ x-sig x-exp y-sig)
  (def x-bits (integer-length x-sig))
  (def y-bits (integer-length y-sig))
  (cond [(or (y-bits . >= . x-exp)
             (y-bits . >= . (- (+ x-exp x-bits) (bf-bits))))
         ; y-sig overlaps or abuts x-sig, or x-sig normalized to (bf-bits); its
         ; value (not just its presence) affects the result, so we must add
         (values (+ (x-sig . << . x-exp) y-sig) 0)]
        [else
         ; y is less than (bfulp x), so only its *nonzero presence* affects the
         ; result: it may change how x rounds when normalized to (bf-bits) bits,
         ; so we append #b01 to the end to affect x the same way
         (def shift (+ 2 (max 0 (- (bf-bits) x-bits))))
         (values (bitwise-ior (x-sig . << . shift) 1) (- x-exp shift))]))

(: abs+ (Natural Integer Natural Integer -> (values Integer Integer)))
(define (abs+ x-sig x-exp y-sig y-exp)
  (cond [(x-exp . > . y-exp)
         (def (values z-sig z-exp)
           (diffexp+ x-sig (cast (- x-exp y-exp) natural?) y-sig))
         (values z-sig (+ z-exp y-exp))]
        [(y-exp . > . x-exp)
         (def (values z-sig z-exp)
           (diffexp+ y-sig (cast (- y-exp x-exp) natural?) x-sig))
         (values z-sig (+ z-exp x-exp))]
        [else
         (values (+ x-sig y-sig) x-exp)]))

(: bf+ (bigfloat bigfloat -> bigfloat))
(define (bf+ x y)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (cond [(zero? x-sgn)  (bfnormalize y)]
        [(zero? y-sgn)  (bfnormalize x)]
        ; cases that are actually subtraction
        ; (x + -y) = (x - y)  or  (-x + y) = -(x - y)
        [(not (= x-sgn y-sgn))  (def (values z-sig z-exp)
                                  (abs- x-sig x-exp y-sig y-exp))
                                (new-bigfloat (* x-sgn z-sig) z-exp)]
        ; now x and y have the same nonzero sign
        ; (x + y)  or  (-x + -y) = -(x + y)
        [else  (def (values z-sig z-exp)
                 (abs+ x-sig x-exp y-sig y-exp))
               (new-bigfloat (* x-sgn z-sig) z-exp)]))

(: bfadd1 (bigfloat -> bigfloat))
(define (bfadd1 x)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (cond [(zero? x-sgn)  (bf 1)]
        ; -x + 1 = -(x - 1)
        [(negative? x-sgn)  (def (values z-sig z-exp)
                              (abs- x-sig x-exp 1 0))
                            (new-bigfloat (- z-sig) z-exp)]
        [else  (def (values z-sig z-exp)
                 (abs+ x-sig x-exp 1 0))
               (new-bigfloat z-sig z-exp)]))

(: diffexp- (Natural Natural Natural -> (values Integer Integer)))
; assumes y-exp = 0
(define (diffexp- x-sig x-exp y-sig)
  (def x-bits (integer-length x-sig))
  (def y-bits (integer-length y-sig))
  (cond [(or (y-bits . >= . x-exp)
             (y-bits . >= . (- (+ x-exp x-bits) (bf-bits))))
         ; y-sig overlaps or abuts x-sig, or x-sig normalized to (bf-bits); its
         ; value (not just its presence) affects the result, so we must subtract
         (values (- (x-sig . << . x-exp) y-sig) 0)]
        [else
         ; y is less than (bfulp x), so only its *nonzero presence* affects the
         ; result: it may change how x rounds when normalized to (bf-bits) bits,
         ; so we append #b-01 to the end to affect x the same way
         (def shift (+ 2 (max 0 (- (bf-bits) x-bits))))
         (values (sub1 (x-sig . << . shift)) (- x-exp shift))]))

(: abs- (Natural Integer Natural Integer -> (values Integer Integer)))
(define (abs- x-sig x-exp y-sig y-exp)
  (cond [(x-exp . > . y-exp)
         (def (values z-sig z-exp)
           (diffexp- x-sig (cast (- x-exp y-exp) natural?) y-sig))
         (values z-sig (+ z-exp y-exp))]
        [(y-exp . > . x-exp)
         (def (values z-sig z-exp)
           (diffexp- y-sig (cast (- y-exp x-exp) natural?) x-sig))
         (values (- z-sig) (+ z-exp x-exp))]
        [else
         (values (- x-sig y-sig) x-exp)]))

(: bf- (bigfloat bigfloat -> bigfloat))
(define (bf- x y)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (cond [(zero? x-sgn)  (bfnormalize (bfneg y))]
        [(zero? y-sgn)  (bfnormalize x)]
        ; cases that are actually addition
        ; (-x - y) = -(x + y)  or  (x - -y) = (x + y)
        [(not (= x-sgn y-sgn))  (def (values z-sig z-exp)
                                  (abs+ x-sig x-exp y-sig y-exp))
                                (new-bigfloat (* x-sgn z-sig) z-exp)]
        ; now x and y have the same nonzero sign
        ; (x - y)  or  (-x - -y) = -(x - y)
        [else  (def (values z-sig z-exp)
                 (abs- x-sig x-exp y-sig y-exp))
               (new-bigfloat (* x-sgn z-sig) z-exp)]))

(: bfsub1 (bigfloat -> bigfloat))
(define (bfsub1 x)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (cond [(zero? x-sgn)  (bf -1)]
        ; -x - 1 = -(x + 1)
        [(negative? x-sgn)  (def (values z-sig z-exp)
                              (abs+ x-sig x-exp 1 0))
                            (new-bigfloat (- z-sig) z-exp)]
        [else  (def (values z-sig z-exp)
                 (abs- x-sig x-exp 1 0))
               (new-bigfloat z-sig z-exp)]))

(: bfneg (bigfloat -> bigfloat))
(define (bfneg x)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat (- x-sig) x-exp))

(: bf* (bigfloat bigfloat -> bigfloat))
;; Computes x*2^e * y*2^f = (x*y)*2^(e+f)
;; Multiplying is SO EASY compared to other arithmetic
(define (bf* x y)
  (def (bigfloat x-sig x-exp) x)
  (def (bigfloat y-sig y-exp) y)
  (cond [(or (zero? x-sig) (zero? y-sig))  (bf 0)]
        [else  (new-bigfloat (* x-sig y-sig) (+ x-exp y-exp))]))

(: bf*i (bigfloat Integer -> bigfloat))
;; Computes x*2^e * i = (x*i)*2^e
(define (bf*i x n)
  (def (bigfloat x-sig x-exp) x)
  (cond [(or (zero? x-sig) (zero? n))  (bf 0)]
        [else  (new-bigfloat (* x-sig n) x-exp)]))

(: bfsqr (bigfloat -> bigfloat))
;; Computes (x*2^e)^2 = x^2 * 2^(2*e)
(define (bfsqr x)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat (sqr x-sig) (*2 x-exp)))

(: remainder->rounding-bits (Natural Natural -> Fixnum))
;; An integer quotient can be used to compute *most* of every floating-point
;; division. The integer remainder affects only the quotients' rounding, and we
;; have a couple of options for how to use it. 1) Inspect the remainder and
;; round the quotient significand based on that, effectively adapting the
;; code in `new-bigfloat' to integers+rationals. Bleah. 2) Append two bits to
;; the unrounded quotient signficand, causing `new-bigfloat' to round it to
;; exactly the same result as in #1.
;; This function computes the two last-place bits for option #2.
(define (remainder->rounding-bits r y)
  (cond [(zero? r)  #b00]
        #:with (def 2r (*2 r))
        [(2r . < . y)  #b01]
        [(2r . = . y)  #b10]
        [else  #b11]))

(: abs/ (Natural Integer Natural Integer -> (values Integer Integer)))
;; Divides two nonnegative bigfloats, given their signficands and exponents.
(define (abs/ x-sig x-exp y-sig y-exp)
  ; number of bits x-sig needs to ensure result has exactly (bf-bits) bits
  (def new-x-bits-ub (+ (integer-bits y-sig) (bf-bits)))
  ; how many bits to shift x-sig leftward to have at least new-x-bits-ub bits
  (def shift (max 0 (- new-x-bits-ub (integer-bits x-sig))))
  (def new-x-sig (x-sig . << . shift))
  (def new-x-exp (- x-exp shift))
  (def (values z-sig z-rem) (quotient/remainder new-x-sig y-sig))
  ; extract rounding bits from the remainder
  (def rounding-bits (remainder->rounding-bits z-rem y-sig))
  ; append the rounding bits to the quotient, subtract exponents, and adjust
  ; the result exponent to account for appending the rounding bits
  (values (bitwise-ior (z-sig . << . 2) rounding-bits)
          (- new-x-exp y-exp 2)))

(: bf/ (bigfloat bigfloat -> bigfloat))
(define (bf/ x y)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (cond [(zero? x-sgn)  (bf 0)]
        [(zero? y-sgn)  (raise-type-error 'bf/ "bigfloat != 0" 1 x y)]
        [else  (def (values z-sig z-exp)
                 (abs/ x-sig x-exp y-sig y-exp))
               (new-bigfloat (if (= x-sgn y-sgn) z-sig (- z-sig)) z-exp)]))

(: bfinv (bigfloat -> bigfloat))
;; bf/ and abs/ specialized to dividend (bf 1)
(define (bfinv y)
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (when (zero? y-sgn) (raise-type-error 'bfinv "bigfloat != 0" 0 y))
  (def y-bits (integer-length y-sig))
  (def x-bits (+ y-bits (bf-bits)))
  (def (values z-sig z-rem) (quotient/remainder (1 . << . x-bits) y-sig))
  (def rounding-bits (remainder->rounding-bits z-rem y-sig))
  (new-bigfloat (* y-sgn (bitwise-ior (z-sig . << . 2) rounding-bits))
                (- (- x-bits) y-exp 2)))

(: bfquotient (bigfloat bigfloat -> bigfloat))
;; Just like bf/, but normalizes *after* truncating
(define (bfquotient x y)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (cond [(zero? x-sgn)  (bf 0)]
        [(zero? y-sgn)  (raise-type-error 'bfquotient "bigfloat != 0" 1 x y)]
        [else  (def (values z-sig z-exp)
                 (abs/ x-sig x-exp y-sig y-exp))
               (bfnormalize
                (bftruncate
                 (bigfloat (if (= x-sgn y-sgn) z-sig (- z-sig)) z-exp)))]))

(: bfexact-quotient (bigfloat bigfloat -> Integer))
;; Calls bfquotient with enough bits to ensure exactness, and returns an exact
;; integer
(define (bfexact-quotient x y)
  (cond
    [((bfabs x) . bf< . (bfabs y))  0]
    [(x . bf= . y)  1]
    [(x . bf= . (bfneg y))  -1]
    [else  (def q-bits (max 2 (- (bfceiling-log2 (bfabs x))
                                 (bffloor-log2 (bfabs y)))))
           (bigfloat->integer
            (parameterize ([bf-bits  (add1 q-bits)])
              (bfquotient x y)))]))

(: bfremainder (bigfloat bigfloat -> bigfloat))
(define (bfremainder x y)
  (cond
    [((bfabs x) . bf< . (bfabs y))  x]
    [((bfabs x) . bf= . (bfabs y))  (bf 0)]
    [else  (def q (bfexact-quotient x y))
           (with-bf-bits (+ (bf-bits) (integer-bits q))
             (bf- x (bf*i y q)))]))

(: bfsqrt (bigfloat -> bigfloat))
#|
Uses sqrt(x*2^y) = sqrt(x) * sqrt(2^y) = sqrt(x) * 2^(y/2). We can represent the
*unrounded* square root of a bigfloat with integer-sqrt(x) * 2^(y/2) if y is
even. We ensure y is even before computing integer-sqrt(x).

For precision: integer-sqrt halves the number of bits it receives as input, so
we give it (bf-bits) * 2 + 1 bits to ensure we get at least (bf-bits) bits in
return.

For rounding, the main idea is the same as in correctly rounded division: use
two rounding bits in last place to represent the rest of the infinitely many
fractional bits. If integer-sqrt is passed four extra bits, it'll output two
extra bits, which we turn into rounding bits. We get the most significant
rounding bit (MSRB) calculated as part of the square root, but the least
significant rounding bit (LSRB) may need to change.

There are three cases to consider:
  1. MSRB is 0. Then the LSRB's value doesn't affect rounding.
  2. MSRB is 1, and the square root *remainder* is zero. Then there are no more
     fractional bits, so with no changes to the LSRB, rounding is correct.
  3. MSRB is 1, and the remainder is nonzero. Then LSRB should be 1 so that
     rounding will account for the remaining fractional bits.
Because of #1, we don't need to check the MSRB before adjusting the LSRB.
|#
(define (bfsqrt x)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (cond [(negative? x-sgn)
         (raise-type-error 'bfsqrt "bigfloat >= 0" 0 x)]
        [(zero? x-sgn)  (bf 0)]
        [else
         (def x-bits (integer-length x-sig))
         ; target bits: (bf-bits) * 2 + 1 ensures the result has (bf-bits) bits;
         ; add four bits to get two rounding bits from integer-sqrt
         (def bits (+ 4 (add1 (* 2 (bf-bits)))))
         (def shift (max 0 (- bits x-bits)))
         ; make sure dividing resulting exponent by two yields an integer
         (def even-shift (if (even? (- x-exp shift)) shift (add1 shift)))
         (def (values z-sig z-rem)
           (integer-sqrt/remainder (cast (x-sig . << . even-shift) positive?)))
         ; adjust least significant rounding bit
         (def rz-sig (if (zero? z-rem) z-sig (bitwise-ior z-sig 1)))
         (new-bigfloat rz-sig (/2 (- x-exp even-shift)))]))

(: bfsum ((Listof bigfloat) -> bigfloat))
;; Source: James Demmel and Yozo Hida. Fast and Accurate Floating Point
;; Summation with Application to Computational Geometry. Numerical Algorithms
;; 2002.
;; Approach: Set number of bits in intermediate results (functional version of
;; the "accumulator") based on number of terms to add, guaranteeing an upper
;; bound on error. The paper has us sorting input on reverse order of floating-
;; point exponents, but I haven't been able to increase error by NOT sorting.
;; So we don't sort, which is the slowest part of the algorithm.
(define (bfsum xs)
  (def n (length xs))
  (cond
    [(= 0 n)  (bf 0)]
    [(= 1 n)  (car xs)]
    [(= 2 n)  (bf+ (car xs) (cadr xs))]
    [else
     ; usually f will be (bf-bits); if it's not, we'll end up summing with more
     ; precision than we need (which is okay)
     (def f (apply max (bf-bits) (map bigfloat-bits xs)))
     ; add a few bits to reduce error (paper targets 1.5 ulps)
     (def F (+ 2 f (ceiling-log2i n)))
     ;(printf "n = ~v~n" (length xs))
     ;(printf "f = ~v, F = ~v~n" f F)
     ;(printf "max n = ~v~n"
     ;        (add1 (floor (/ (expt 2 (- F f)) (- 1 (expt 2 (- f)))))))
     (with-bf-bits F
       (foldl bf+ (car xs) (cdr xs)))]))
