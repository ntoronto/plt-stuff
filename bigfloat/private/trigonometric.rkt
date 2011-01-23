#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bfexp-terms-ub.rkt" "bf-pi.rkt" "binary-split.rkt")

(require/typed
 racket/base
 [integer-sqrt  (Natural -> Natural)])

(provide bfcos bfsin bftan)

(: reduce-theta (bigfloat -> (values bigfloat Integer Integer)))
(define (reduce-theta z)
  (parameterize ([bf-bits  (+ (bf-bits)
                              (max 0 (bfceiling-log2 (bfadd1 (bfabs z)))))])
    (def pi (bf-pi))
    (def pi*2  (bf*2 pi))
    (def y (if ((bfabs z) . bf< . pi*2) z (bfremainder z pi*2)))
    (def (values x sinsgn)
      (if (bfnegative? y) (values (bfneg y) -1) (values y 1)))
    ; now 0 <= x < pi*2
    (cond [(x . bf< . (bf/2 pi))          (values (bfnormalize x) 1 sinsgn)]
          [(x . bf< . pi)                 (values (bf- pi x) -1 sinsgn)]
          [(x . bf< . (bf* pi (bf 3/2)))  (values (bf- x pi) -1 (- sinsgn))]
          [else                           (values (bf- pi*2 x) 1 (- sinsgn))])))

(: cos-taylor-sum/binary-split (bigfloat -> bigfloat))
(define (cos-taylor-sum/binary-split x)
  (def N (+ 2 (/2 (bfexp-terms-ub x (max 1 (- (bflog2-ulp x)))))))
  ;(printf "N = ~v~n" N)
  (def -x^2 (bfneg (bfsqr x)))
  (define-syntax-rule (p n) (if (zero? n) (bf 1) -x^2))
  (define-syntax-rule (q n) (if (zero? n) 1 (let ([2n  (*2 n)])
                                              (* 2n (sub1 2n)))))
  (def (values u v) (binary-split/pq* bigfloat Integer bf+ bf* * bf*i p q N))
  (bf/ u (bf v)))

(: sin-taylor-sum/binary-split (bigfloat -> bigfloat))
(define (sin-taylor-sum/binary-split x)
  (def N (+ 2 (/2 (bfexp-terms-ub x (max 1 (- (bflog2-ulp x)))))))
  ;(printf "N = ~v~n" N)
  (def -x^2 (bfneg (bfsqr x)))
  (define-syntax-rule (p n) (if (zero? n) x -x^2))
  (define-syntax-rule (q n) (if (zero? n) 1 (let ([2n  (*2 n)])
                                              (* 2n (add1 2n)))))
  (def (values u v) (binary-split/pq* bigfloat Integer bf+ bf* * bf*i p q N))
  (bf/ u (bf v)))

(: best-k (bigfloat -> Natural))
(define (best-k y)
  (max 0 (+ (if (bfzero? y) 0 (bfceiling-log2 (bfabs y)))
            (quotient (integer-sqrt (bf-bits)) 2))))

(: bfcos* (bigfloat -> bigfloat))
(define (bfcos* y)
  ; reduce using cos(y) = 2*cos(y/2)^2 - 1
  (def k (best-k y))
  ;(printf "(bfcos*) k = ~v~n" k)
  (def x (y . bf/2^ . k))
  ;(printf "(bfcos*) x = ~v~n" x)
  (parameterize ([bf-bits  (+ (bf-bits) (* 2 k))])
    (def cos-x (cos-taylor-sum/binary-split x))
    (let loop ([cos-x cos-x] [k k])
      (cond [(zero? k)  cos-x]
            [else (loop (bfsub1 (bf*2 (bfsqr cos-x)))
                        (sub1 k))]))))

(: bfsin* (bigfloat -> bigfloat))
(define (bfsin* y)
  ; reduce using sin(y) = 3*sin(y/3) - 4*sin(y/3)^3
  (def k (quotient (*2 (best-k y)) 3))
  ;(printf "(bfsin*) k = ~v~n" k)
  (parameterize ([bf-bits  (+ (bf-bits) (* 2 k))])
    (def x (bf/ y (bf (expt 3 k))))
    ;(printf "(bfsin*) x = ~v~n" x)
    (def sin-x (sin-taylor-sum/binary-split x))
    (let loop ([sin-x sin-x] [k k])
      (cond [(zero? k)  sin-x]
            [else  (loop (bf- (bf* (bf 3) sin-x)
                              (bf* (bf 4) (bf* sin-x (bfsqr sin-x))))
                         (sub1 k))]))))

(: pi/2-minus (bigfloat -> bigfloat))
(define (pi/2-minus y)
  (def x (bf- (bf/2 (bf-pi)) y))
  (def k (if (bfzero? x) (bf-bits) (max 0 (- (bffloor-log2 (bfabs x))))))
  (with-bf-bits (+ (bf-bits) k)
    (bf- (bf/2 (bf-pi)) y)))

(: bfcos (bigfloat -> bigfloat))
(define (bfcos z)
  (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
    (def (values y sgn _) (reduce-theta z))
    (def pi/4 ((bf-pi) . bf/2^ . 2))
    (cond [(y . bf< . pi/4)  (bf*i (bfcos* y) sgn)]
          [else  (bf*i (bfsin* (pi/2-minus y)) sgn)])))

(: bfsin (bigfloat -> bigfloat))
(define (bfsin z)
  (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
    (def (values y _ sgn) (reduce-theta z))
    (def pi/4 ((bf-pi) . bf/2^ . 2))
    (cond [(y . bf< . pi/4)  (bf*i (bfsin* y) sgn)]
          [else  (bf*i (bfcos* (pi/2-minus y)) sgn)])))

(: bftan (bigfloat -> bigfloat))
(define (bftan z)
  (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits)))
    (def (values y cos-sgn sin-sgn) (reduce-theta z))
    (def pi/2 (bf/2 (bf-pi)))
    (def (values sin-z cos-z)
      (cond [(y . bf< . (bf/2 pi/2))
             ; the numerator will be smaller than the denominator, so error in
             ; the numerator will have a greater effect on the quotient
             (def sin-z (bf*i (bfsin* y) sin-sgn))
             (def cos-z (bf*i (bfsqrt (bf- (bf 1) (bfsqr sin-z))) cos-sgn))
             (values sin-z cos-z)]
            [else
             ; the numerator will be bigger than the denominator, so error in
             ; the denominator will have a greater effect on the quotient
             (def cos-z (bf*i (bfsin* (pi/2-minus y)) cos-sgn))
             (def sin-z (bf*i (bfsqrt (bf- (bf 1) (bfsqr cos-z))) sin-sgn))
             (values sin-z cos-z)]))
    (when (bfzero? cos-z)
      (error 'bftan "(bftan ~e) is undefined" z))
    (bf/ sin-z cos-z)))
