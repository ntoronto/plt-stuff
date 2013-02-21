#lang typed/racket/base

(require (except-in racket/flonum fl->exact-integer)
         "utils.rkt")

(require/typed
 racket/flonum
 [fl->exact-integer  (Float -> Integer)])

(provide (except-out (all-defined-out)
                     decimal-float->bigfloat ensure-digits string->bigfloat/fields
                     bigfloat->decimal-float remove-trailing-zeros format-exp format-sci
                     shift-compare))

(: bf-bits (Parameterof Exact-Positive-Integer))
(define bf-bits
  (make-parameter
   128 (λ: ([n : Exact-Positive-Integer])
           (if (n . < . 2) 2 n))))

; exponent after which we require a bigfloat->big-decimal-float handler, and
; start printing bigfloats as (bf "...") instead of (bf #e...)
(define binary-exp-cutoff 33220)
; exponent after which we bail on parsing decimals
(define decimal-exp-cutoff 10000)

(define-struct: bigfloat ([sig : Integer] [exp : Integer])
  #:property prop:custom-write
  (λ (x port mode)
    (display
     (cond [((abs (bigfloat-exp x)) . > . binary-exp-cutoff)
            (format "(bf \"~a\")" (bigfloat->string x))]
           [else
            (format "(bf #e~a)" (bigfloat->string x))])
     port))
  #:property prop:equal+hash
  (let ()
    (: bigfloat-hash (bigfloat (Any -> Integer) -> Integer))
    (define (bigfloat-hash x rec-hash)
      (def (bigfloat x-sig x-exp) (bfcanonicalize x))
      (bitwise-xor (rec-hash x-sig) (rec-hash x-exp)))
    (list (λ (x y _) (bf= x y)) bigfloat-hash bigfloat-hash)))

(: bigfloat-bits (bigfloat -> Nonnegative-Fixnum))
(define (bigfloat-bits x)
  (integer-bits (bigfloat-sig x)))

(: bigfloat->fields/sign (bigfloat -> (values Fixnum Natural Integer)))
(define (bigfloat->fields/sign x)
  (def (bigfloat x-sig x-exp) x)
  (cond [(positive? x-sig)  (values 1 x-sig x-exp)]
        [(negative? x-sig)  (values -1 (abs x-sig) x-exp)]
        [else  (values 0 0 0)]))

(: bflog2-ulp (bigfloat -> Integer))
(define bflog2-ulp bigfloat-exp)

(: bfulp (bigfloat -> Exact-Rational))
(define (bfulp x)
  (expt 2 (bflog2-ulp x)))

(: new-bigfloat (Integer Integer -> bigfloat))
(define (new-bigfloat x-sig x-exp)
  (cond [(zero? x-sig)  (bigfloat 0 0)]
        #:with (def underflow (- (bf-bits) (integer-bits x-sig)))
        [(zero? underflow)      (bigfloat x-sig x-exp)]
        [(positive? underflow)  (bigfloat (x-sig . << . underflow)
                                          (- x-exp underflow))]
        #:with (def (values new-sig adj)
                 (rounding-shift x-sig underflow))
        [(zero? adj)  (bigfloat new-sig (- x-exp underflow))]
        [else         (bigfloat (new-sig . << . adj) (- x-exp underflow adj))]))

(: bfnormalize (bigfloat -> bigfloat))
(define (bfnormalize x)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat x-sig x-exp))

(: bfcanonicalize (bigfloat -> bigfloat))
(define (bfcanonicalize x)
  (cond [(bfzero? x)  (bigfloat 0 0)]
        #:with (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
        #:with (def shift (bitwise-first-bit-set x-sig))
        [(positive? shift)
         (bigfloat (* x-sgn (x-sig . << . (- shift))) (+ x-exp shift))]
        [else  x]))

;; =============================================================================
;; float conversion

(: float->bigfloat (Float -> bigfloat))
; about 2.5x faster than converting to rational and using rational->bigfloat
(define (float->bigfloat x)
  (def (values x-sig x-exp) (float->float-fields x))
  (new-bigfloat x-sig x-exp))

(: bigfloat->float (bigfloat -> Float))
(define (bigfloat->float x)
  (def (bigfloat x-sig x-exp) x)
  (def shift (- 53 (integer-bits x-sig)))
  (def (values y-sig y-exp)
    (def (values y-sig adj)
      (rounding-shift x-sig shift))
    (if (zero? adj)
        (values y-sig (- x-exp shift))
        (values (y-sig . << . adj) (- x-exp shift adj))))
  (float-fields->float y-sig y-exp))

;; =============================================================================
;; integer conversion

(: integer->bigfloat (Integer -> bigfloat))
(define (integer->bigfloat x)
  (new-bigfloat x 0))

(: bigfloat->integer (bigfloat -> Integer))
(define (bigfloat->integer x)
  (def (bigfloat x-sig x-exp) x)
  (def res (x-sig . << . x-exp))
  (if (and (negative? x-exp) (negative? x-sig)
           (not (zero? (bitwise-low-bits x-sig (- x-exp)))))
      (add1 res)
      res))

;; =============================================================================
;; rational conversion

(: rational->bigfloat (Exact-Rational -> bigfloat))
(define (rational->bigfloat x)
  (def bits (bf-bits))
  (def x-sgn (sgn x))
  (def ax (inexact->exact (abs x)))
  (def ipart (floor ax))
  (cond [(zero? x-sgn)  (bigfloat 0 0)]
        [(zero? ipart)  (def e (- (log2-lb ax) bits -1))
                        (bigfloat (* x-sgn (round (* ax (2^ (- e))))) e)]
        #:with (def e (- (integer-length ipart) bits))
        [(e . >= . 0)   (bigfloat (* x-sgn (round (/ ax (2^ e)))) e)]
        [else           (bigfloat (* x-sgn (round (* ax (2^ (- e))))) e)]))

(: bigfloat->rational (bigfloat -> Exact-Rational))
(define (bigfloat->rational x)
  (def (bigfloat x-sig x-exp) x)
  (if (negative? x-exp)
      (/ x-sig (2^ (- x-exp)))
      (* x-sig (2^ x-exp))))

;; =============================================================================
;; conversion convenience functions

(: real->bigfloat (Real -> bigfloat))
(define (real->bigfloat x)
  (cond [(exact-integer? x)  (integer->bigfloat x)]
        [(flonum? x)         (float->bigfloat x)]
        [else                (rational->bigfloat (inexact->exact x))]))

(: bf ((U String Real) -> bigfloat))
(define (bf x)
  (cond [(string? x)  (def bx (string->bigfloat x))
                      (if (not bx) (error 'bf "not a bigfloat: ~e" x) bx)]
        [else         (real->bigfloat x)]))

;; =============================================================================
;; conversion: decimal/scientific string to bigfloat

(: bf-use-small-string-conversion (Parameterof Boolean))
(define bf-use-small-string-conversion (make-parameter #t))

(: big-decimal-float->bigfloat-box
   (Boxof (U False (Fixnum Natural Integer -> bigfloat))))
(define big-decimal-float->bigfloat-box (box #f))

(: ensure-digits (Natural Integer Integer -> (values Natural Integer)))
(define (ensure-digits x-sig x-exp digits)
  (def underflow (- digits (truncate (log10-ub x-sig))))
  (if (underflow . > . 0)
      (values (* x-sig (expt 10 underflow)) (- x-exp underflow))
      (values x-sig x-exp)))

(: decimal-float->bigfloat (Fixnum Natural Integer -> bigfloat))
(define (decimal-float->bigfloat z-sgn z-sig z-exp)
  (def (values y-sig y-exp)
    (ensure-digits z-sig z-exp (bits->digits (bf-bits))))
  (cond [(zero? y-sig)  (bigfloat 0 0)]
        [(and (bf-use-small-string-conversion)
              ((abs y-exp) . <= . decimal-exp-cutoff))
         (def y (* y-sig (expt 10 y-exp)))
         ; truncate-multiply by log(10)/log(2), adjust for future significand
         (def x-exp
           (- (quotient (* y-exp 20739842733593688) 6243314768165359)
              (bf-bits)))
         (def x-sig (round (if (x-exp . < . 0)
                               (* y (2^ (- x-exp)))
                               (/ y (2^ x-exp)))))
         (new-bigfloat (* z-sgn x-sig) x-exp)]
        #:with (def big-decimal-float->bigfloat
                 (unbox big-decimal-float->bigfloat-box))
        [big-decimal-float->bigfloat
         (big-decimal-float->bigfloat z-sgn y-sig y-exp)]
        [else
         (error 'decimal-float->bigfloat
                "no big-decimal-float->bigfloat handler is installed")]))

(: string->bigfloat/fields (String -> (U bigfloat False)))
(define (string->bigfloat/fields str)
  (def m (regexp-match #rx"^([+-]?)([0-9]+\\*)?(2\\^[+-]?[0-9]+)?$" str))
  ;(printf "m = ~v~n" m)
  (cond [(or (not m) (let ([mstr  (car m)])
                       (or (not mstr) (= 0 (string-length mstr)))))  #f]
        [else
         (def (list _ sgnstr istr estr) m)
         (def sgn (if (and sgnstr (string=? "-" sgnstr)) -1 1))
         (def sig (if istr
                      (cast (string->number
                             (substring istr 0 (sub1 (string-length istr))))
                            natural?)
                      1))
         (def exp (if estr (cast (string->number (substring estr 2))
                                 exact-integer?) 0))
         (new-bigfloat (* sgn sig) exp)]))

(: string->bigfloat (String -> (U bigfloat False)))
(define (string->bigfloat str)
  (def m (regexp-match #rx"^([+-]?)([0-9]*)\\.?([0-9]*)(e[+-]?[0-9]+)?$" str))
  ;(printf "m = ~v~n" m)
  (cond [(or (not m) (let ([mstr  (car m)])
                       (or (not mstr) (= 0 (string-length mstr)))))
         (string->bigfloat/fields str)]
        #:with (def (list _ sgnstr istr fstr expstr) m)
        [(not (and istr fstr))  #f]  ; keeps TR happy...
        [(= 0 (+ (string-length istr) (string-length fstr)))  #f]
        [else
         (def sgn (if (and sgnstr (string=? "-" sgnstr)) -1 1))
         (def sig (cast (string->number (string-append istr fstr)) natural?))
         (def texp (cast (if expstr (string->number (substring expstr 1)) 0)
                         exact-integer?))
         (def exp (- texp (string-length fstr)))
         (decimal-float->bigfloat sgn sig exp)]))

;; =============================================================================
;; conversion: bigfloat to decimal/scientific string

(: bigfloat->big-decimal-float-box
   (Boxof (U False (bigfloat -> (values Fixnum Integer Integer)))))
(define bigfloat->big-decimal-float-box (box #f))

(: bigfloat->decimal-float (bigfloat -> (values Fixnum Integer Integer)))
(define (bigfloat->decimal-float x)
  (def (bigfloat x-sig x-exp) x)
  (cond [(zero? x-sig) (values 0 0 0)]
        [(and (bf-use-small-string-conversion)
              ((abs x-exp) . <= . binary-exp-cutoff))
         ; truncate-multiply by log(2)/log(10), subtract 1 to ensure enough
         ; digits get into the result
         (def y-exp
           (sub1 (quotient (* x-exp 6243314768165359) 20739842733593688)))
         (def y-sig (round (/ (bigfloat->rational (bigfloat x-sig x-exp))
                               (expt 10 y-exp))))
         (values (sgn y-sig) (abs y-sig) y-exp)]
        #:with (def bigfloat->big-decimal-float
                 (unbox bigfloat->big-decimal-float-box))
        [bigfloat->big-decimal-float  (bigfloat->big-decimal-float x)]
        [else
         (error 'bigfloat->decimal-float
                "no bigfloat->big-decimal-float handler is installed")]))

(: remove-trailing-zeros (String -> String))
(define (remove-trailing-zeros str)
  (def strlen (string-length str))
  (let loop ([i  (sub1 strlen)])
    (cond [(i . <= . 0)  (substring str 0 1)]
          [(char=? (string-ref str i) #\0)  (loop (sub1 i))]
          [else  (substring str 0 (add1 i))])))

(: format-exp (Integer -> String))
(define (format-exp e)
  (format "e~a~a" (if (negative? e) "-" "+") (abs e)))

(: format-sci (String String Integer -> String))
(define (format-sci sgnstr numstr e)
  (def numstrlen (string-length numstr))
  (if (numstrlen . = . 1)
      (format "~a~a.0~a" sgnstr numstr (format-exp e))
      (format "~a~a.~a~a" sgnstr
              (substring numstr 0 1)
              (remove-trailing-zeros (substring numstr 1))
              (format-exp (+ e numstrlen -1)))))

(: bigfloat->string (bigfloat -> String))
(define (bigfloat->string x)
  (cond
    [(bfzero? x)  "0.0"]
    [(and (not (unbox bigfloat->big-decimal-float-box))
          ((abs (bigfloat-exp x)) . > . binary-exp-cutoff))
     (bigfloat-fields->string x)]
    #:with (begin (def (values y-sgn y-sig y-exp)
                    (bigfloat->decimal-float x))
                  (def sgnstr (if (negative? y-sgn) "-" ""))
                  (def numstr (number->string y-sig)))
    [(zero? y-exp)  (format "~a~a.0" sgnstr numstr)]
    #:with (begin (def digits (string-length numstr))
                  (def point (+ digits y-exp)))
    [(negative? y-exp)
     (cond [(point . <= . (- (max 5 (round (/ digits 3)))))
            (format-sci sgnstr numstr y-exp)]
           [(point . <= . 0)
            (format "~a0.~a~a" sgnstr (make-string (- point) #\0)
                    (remove-trailing-zeros numstr))]
           [else  (format "~a~a.~a" sgnstr
                          (substring numstr 0 point)
                          (remove-trailing-zeros (substring numstr point)))])]
    [((+ (string-length numstr) y-exp) . <= . digits)
     (format "~a~a~a.0" sgnstr numstr (make-string y-exp #\0))]
    [else  (format-sci sgnstr numstr y-exp)]))

(: bigfloat-fields->string (bigfloat -> String))
(define (bigfloat-fields->string x)
  (def (bigfloat x-sig x-exp) x)
  (format "~a*2^~a" x-sig x-exp))

;; =============================================================================
;; predicates, sign, absolute value, comparison

(: bfsgn (bigfloat -> Fixnum))
(define (bfsgn x)
  (sgn (bigfloat-sig x)))

(: bfzero? (bigfloat -> Boolean))
(define (bfzero? x)
  (zero? (bigfloat-sig x)))

(: bfnegative? (bigfloat -> Boolean))
(define (bfnegative? x)
  (negative? (bigfloat-sig x)))

(: bfpositive? (bigfloat -> Boolean))
(define (bfpositive? x)
  (positive? (bigfloat-sig x)))

(: bfinteger? (bigfloat -> Boolean))
(define (bfinteger? x)
  (def (bigfloat x-sig x-exp) x)
  (zero? (bitwise-low-bits (abs x-sig) (- x-exp))))

(: bfabs (bigfloat -> bigfloat))
(define (bfabs x)
  (def (bigfloat x-sig x-exp) x)
  (bigfloat (abs x-sig) x-exp))

#|
All the comparison functions proceed coarse-to-fine. The coarse tests can be
done quickly regardless of difference in argument magnitude. If they do not
decide the comparison, the arguments are close enough that a fine comparison
should be time- and memory- efficient.

1. Compare based on signs of arguments. Critical: make sure that if sign tests
don't decide the comparison, the arguments are proven nonzero.

2. Compare log2 of arguments. Critical: make sure that if log2 tests don't
decide the comparison, the arguments are ``close''.

3. Left-shift one argument to have the same exponent as the other, turning the
problem into a fixed-point comparison, which uses integer ops.
|#

;; For doing #3:
(define-syntax-rule (shift-compare op x-sig* x-exp* y-sig* y-exp*)
  (let ([x-sig x-sig*] [x-exp x-exp*] [y-sig y-sig*] [y-exp y-exp*])
    (cond [(x-exp . > . y-exp)  (op (x-sig . << . (- x-exp y-exp)) y-sig)]
          [(x-exp . < . y-exp)  (op x-sig (y-sig . << . (- y-exp x-exp)))]
          [else                 (op x-sig y-sig)])))

(: bf= (bigfloat bigfloat -> Boolean))
(define (bf= x y)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (cond [(not (= x-sgn y-sgn))  #f]
        [(and (zero? x-sgn) (zero? y-sgn))  #t]
        ; x-sgn = y-sgn != 0, but we don't need the sign to decide equality
        #:with (def flog2x (+ (floor-log2i (cast x-sig positive?)) x-exp))
        #:with (def clog2y (+ (ceiling-log2i (cast y-sig positive?)) y-exp))
        [(flog2x . > . clog2y)  #f]
        #:with (def flog2y (+ (floor-log2i (cast y-sig positive?)) y-exp))
        #:with (def clog2x (+ (ceiling-log2i (cast x-sig positive?)) x-exp))
        [(flog2y . > . clog2x)  #f]
        ; log2s of the arguments are within 1 of each other
        [else  (shift-compare = x-sig x-exp y-sig y-exp)]))

(: bf> (bigfloat bigfloat -> Boolean))
(define (bf> x y)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (cond
    [(x-sgn . > . y-sgn)  #t]
    [(x-sgn . < . y-sgn)  #f]
    [(and (zero? x-sgn) (zero? y-sgn))  #f]
    ; x-sgn = y-sgn != 0
    #:with (def flog2x (+ (floor-log2i (cast x-sig positive?)) x-exp))
    #:with (def clog2y (+ (ceiling-log2i (cast y-sig positive?)) y-exp))
    [(and (= 1 x-sgn) (flog2x . > . clog2y))  #t]
    [(and (= -1 x-sgn) (flog2x . >= . clog2y))  #f]
    #:with (def clog2x (+ (ceiling-log2i (cast x-sig positive?)) x-exp))
    #:with (def flog2y (+ (floor-log2i (cast y-sig positive?)) y-exp))
    [(and (= 1 x-sgn) (clog2x . <= . flog2y))  #f]
    [(and (= -1 x-sgn) (clog2x . < . flog2y))  #t]
    ; log2s of the arguments are within 1 of each other
    [else  (shift-compare > (* x-sgn x-sig) x-exp (* y-sgn y-sig) y-exp)]))

(: bf>= (bigfloat bigfloat -> Boolean))
(define (bf>= x y)
  (def (values x-sgn x-sig x-exp) (bigfloat->fields/sign x))
  (def (values y-sgn y-sig y-exp) (bigfloat->fields/sign y))
  (cond
    [(x-sgn . > . y-sgn)  #t]
    [(x-sgn . < . y-sgn)  #f]
    [(and (zero? x-sgn) (zero? y-sgn))  #t]
    ; x-sgn = y-sgn != 0
    #:with (def flog2x (+ (floor-log2i (cast x-sig positive?)) x-exp))
    #:with (def clog2y (+ (ceiling-log2i (cast y-sig positive?)) y-exp))
    [(and (= 1 x-sgn) (flog2x . > . clog2y))  #t]
    [(and (= -1 x-sgn) (flog2x . > . clog2y))  #f]
    #:with (def clog2x (+ (ceiling-log2i (cast x-sig positive?)) x-exp))
    #:with (def flog2y (+ (floor-log2i (cast y-sig positive?)) y-exp))
    [(and (= 1 x-sgn) (clog2x . < . flog2y))  #f]
    [(and (= -1 x-sgn) (clog2x . < . flog2y))  #t]
    ; log2s of the arguments are within 1 of each other
    [else  (shift-compare >= (* x-sgn x-sig) x-exp (* y-sgn y-sig) y-exp)]))

(: bf!= (bigfloat bigfloat -> Boolean))
(define (bf!= x y) (not (bf= x y)))

(: bf<= (bigfloat bigfloat -> Boolean))
(define (bf<= x y) (not (bf> x y)))

(: bf< (bigfloat bigfloat -> Boolean))
(define (bf< x y) (not (bf>= x y)))
