#lang typed/racket/base

(require (for-syntax racket/base)
         "utils-syntax.rkt")

(require/typed
 racket/base
 [real->floating-point-bytes  (Float 8 -> Bytes)]
 [integer-bytes->integer  (Bytes #f -> Integer)]
 [integer->integer-bytes  (Integer 8 #f -> Bytes)]
 [floating-point-bytes->real  (Bytes -> Float)])

(require/typed
 rnrs/arithmetic/bitwise-6
 [bitwise-first-bit-set  (Integer -> Integer)])

(require/typed
 ffi/unsafe
 [opaque CType ctype?]
 [_long  CType]
 [ctype-sizeof  (CType -> Positive-Fixnum)])

(provide (rename-out [:bitwise-first-bit-set bitwise-first-bit-set]
                     [cond* cond])
         def
         (except-out (all-defined-out)
                     :bitwise-first-bit-set))

(define natural? exact-nonnegative-integer?)
(define natural+? exact-positive-integer?)

(define-type Natural+ Exact-Positive-Integer)

(define-syntax-rule (cast x pred?)
  (let ([y x])
    (if (pred? y) y (raise-type-error 'cast (format "~a" 'pred?) y))))

(: bits->digits (Natural -> Natural))
(define (bits->digits bits)
  (add1 (quotient (* bits 150514997831990597606869447362247)
                  500000000000000000000000000000000)))

(define-syntax-rule (bitwise-ones n) (sub1 (1 . << . n)))
(define-syntax-rule (bitwise-low-bits x n)
  (bitwise-and x (bitwise-ones n)))
(define-syntax-rule (bitwise-high-bits x n)
  (bitwise-and x (bitwise-not (bitwise-ones n))))

(: integer-bits (Integer -> Nonnegative-Fixnum))
(define (integer-bits i)
  (integer-length (abs i)))

(: sgn (Exact-Rational -> Fixnum))
(define (sgn i)
  (cond [(negative? i)  -1]
        [(positive? i)   1]
        [else  0]))

(define << arithmetic-shift)

(: rounding-shift (Integer Integer -> (values Integer Integer)))
;; Performs arithmetic shift; when n is negative, rounds toward even instead
;; of flooring.
;; A rounding shift may return a number whose length is not the length of x
;; plus n. So this function also returns how much an additional shift would
;; have to do to make that happen.
(define (rounding-shift x n)
  (def res
    (cond [(zero? n)  x]
          [(positive? n)  (x . << . n)]
          [else
           (def ipart (x . << . n))
           ; the following stuff magically works on negative numbers
           (def fpart-last-index (sub1 (- n)))
           ; is the fractional part >= 0.5? if so, we check for rounding
           (if (bitwise-bit-set? x fpart-last-index)
               (if (bitwise-bit-set? ipart 0)
                   ; truncating gives an odd number, and we know the fractional
                   ; part is >= 0.5, so round toward even
                   (add1 ipart)
                   (if (zero? (bitwise-low-bits x fpart-last-index))
                       ; the fractional part is only = 0.5, so stay on even
                       ipart
                       ; the fractional part is > 0.5, so round away from even
                       (add1 ipart)))
               ipart)]))
  (values res (- (+ (integer-bits x) n)
                 (integer-bits res))))

(define-syntax-rule (<<r x n) (let-values ([(y _) (rounding-shift x n)]) y))

(define-syntax-rule (*2 n) (n . << . 1))
(define-syntax-rule (*4 n) (n . << . 2))
(define-syntax-rule (/2 n) (n . << . -1))
(define-syntax-rule (/4 n) (n . << . -2))
(define-syntax-rule (2^ n) (1 . << . n))
(define-syntax-rule (*2^ x n) (x . << . n))
(define-syntax-rule (/2^ x n) (x . << . (- n)))

(define-syntax-rule (fix* x y e) ((* x y) . <<r . e))
(define-syntax-rule (fixsqr x e) (let ([y x]) (fix* y y e)))
(define-syntax-rule (fix/ x y e) (quotient (x . << . (- e)) y))
(define-syntax-rule (fix1/ x e) (quotient (1 . << . (*2 (- e))) x))
(define-syntax-rule (fixsqrt x e) (integer-sqrt (x . << . (- e))))

(: floor-log2i (Exact-Positive-Integer -> Natural))
(define (floor-log2i n)
  (if (negative? n)
      (raise-type-error 'floor-log2i "Natural" n)
      (max 0 (sub1 (integer-length n)))))

(: ceiling-log2i (Exact-Positive-Integer -> Natural))
(define (ceiling-log2i n)
  (if (negative? n)
      (raise-type-error 'ceiling-log2i "Natural" n)
      (max 0 (integer-length (sub1 n)))))

(: log2-ub (Exact-Rational -> Integer))
(define (log2-ub x)
  (with-asserts ([x exact?] [x positive?])
    (def u (numerator x))
    (def v (denominator x))
    (with-asserts ([u exact-positive-integer?] [v exact-positive-integer?])
      (- (ceiling-log2i u) (floor-log2i v)))))

(: log2-lb (Exact-Rational -> Integer))
(define (log2-lb x)
  (with-asserts ([x exact?] [x positive?])
    (def u (numerator x))
    (def v (denominator x))
    (with-asserts ([u exact-positive-integer?] [v exact-positive-integer?])
      (- (floor-log2i u) (ceiling-log2i v)))))

(: log-ub (Exact-Rational -> Exact-Rational))
(define (log-ub x)
  (with-asserts ([x exact?] [x positive?])
    (def u (numerator x))
    (def v (denominator x))
    (with-asserts ([u exact-positive-integer?] [v exact-positive-integer?])
      (- (* (ceiling-log2i u) #e0.693147180559946)
         (* (floor-log2i v) #e0.693147180559945)))))

(: log-lb (Exact-Rational -> Exact-Rational))
(define (log-lb x)
  (with-asserts ([x exact?] [x positive?])
    (def u (numerator x))
    (def v (denominator x))
    (with-asserts ([u exact-positive-integer?] [v exact-positive-integer?])
      (- (* (floor-log2i u) #e0.693147180559945)
         (* (ceiling-log2i v) #e0.693147180559946)))))

(: log10-ub (Exact-Rational -> Exact-Rational))
(define (log10-ub x)
  (with-asserts ([x exact?] [x positive?])
    (def u (numerator x))
    (def v (denominator x))
    (with-asserts ([u exact-positive-integer?] [v exact-positive-integer?])
      (- (* (ceiling-log2i u) #e0.30102999566398115)
         (* (floor-log2i v) #e0.30102999566398114)))))

(: log10-lb (Exact-Rational -> Exact-Rational))
(define (log10-lb x)
  (with-asserts ([x exact?] [x positive?])
    (def u (numerator x))
    (def v (denominator x))
    (with-asserts ([u exact-positive-integer?] [v exact-positive-integer?])
      (- (* (floor-log2i u) #e0.30102999566398114)
         (* (ceiling-log2i v) #e0.30102999566398115)))))

(: float->bit-field (Float -> Integer))
(define (float->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 8) #f))

(: bit-field->float (Integer -> Float))
(define (bit-field->float i)
  (floating-point-bytes->real (integer->integer-bytes i 8 #f)))

(define float-neg-one (1 . << . 63))
(define float-sig-one (1 . << . 52))
(define float-sig-mask (bitwise-ones 52))
(define float-bad-exp #b11111111111)

(: float->float-fields (Float -> (values Integer Integer)))
(define (float->float-fields x)
  (def i (float->bit-field x))
  (def x-sig-bits (cast (bitwise-and i float-sig-mask) natural?))
  (def x-exp-bits (bitwise-bit-field i 52 63))
  (def x-sgn? (bitwise-bit-set? i 63))
  (cond [(= x-exp-bits float-bad-exp)
         (error 'float->float-fields "~e is not rational" x)]
        [(zero? x-exp-bits)
         ; subnormal floats (including zero)
         (if (zero? x-sig-bits)
             (values 0 0)
             (values (if x-sgn? (- x-sig-bits) x-sig-bits) -1074))]
        [else  (def x-sig (bitwise-ior x-sig-bits float-sig-one))
               (values (if x-sgn? (- x-sig) x-sig) (- x-exp-bits 1075))]))

(: float-fields->float (Integer Integer -> Float))
(define (float-fields->float x-sig x-exp)
  (cond
    [(x-exp . < . -1023)  0.0]
    [(x-exp . > . 1024)  (if (negative? x-sig) -inf.0 +inf.0)]
    [else  (bit-field->float
            (bitwise-ior
             (if (negative? x-sig) float-neg-one 0)
             (bitwise-and (abs x-sig) float-sig-mask)
             ((bitwise-and (+ x-exp 1075) float-bad-exp) . << . 52)))]))

(define fixnum-size (- (* 8 (ctype-sizeof _long)) 2))

(: :bitwise-first-bit-set (Integer -> Integer))
; need this because bitwise-first-bit-set in the library is absolutely pitiful
; on numbers that don't fit in a fixnum
(define (:bitwise-first-bit-set i)
  (cond [(zero? i)  -1]
        [(fixnum? i)  (bitwise-first-bit-set i)]
        [else
         (let loop ([start 0] [size fixnum-size])
           (if (zero? size)
               start
               (let ([stop  (+ start size)])
                 (if (zero? (bitwise-bit-field i start stop))
                     (loop stop size)
                     (loop start (arithmetic-shift size -1))))))]))
