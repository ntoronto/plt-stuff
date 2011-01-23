#lang typed/racket/base

(require (except-in racket/flonum fl->exact-integer)
         "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bflog.rkt" "bfexp-terms-ub.rkt" "binary-split.rkt")

(require/typed
 racket/base
 [integer-sqrt  (Natural -> Natural)])

(require/typed
 racket/flonum
 [fl->exact-integer  (Float -> Integer)])

(provide bfexp bf-e)

(: taylor-sum/binary-split (bigfloat -> bigfloat))
(define (taylor-sum/binary-split x)
  (def N (bfexp-terms-ub x (max 1 (- (bflog2-ulp x)))))
  (def one (bf 1))
  ;(printf "N = ~v~n" N)
  (define-syntax-rule (p n) (if (zero? n) one x))
  (define-syntax-rule (q n) (if (zero? n) 1 n))
  (define-values (ru rv)
    (binary-split/pq* bigfloat Integer bf+ bf* * bf*i p q N))
  (bf/ ru (bf rv)))

(: sqr* (bigfloat Natural -> bigfloat))
(define (sqr* x k)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat
   (let loop ([x-sig x-sig] [k k])
     (if (zero? k)
         x-sig
         (loop (fixsqr x-sig x-exp) (sub1 k))))
   x-exp))

#|
x^0 * (a0 + a1 * x^1 + a2 * x^2)
x^3 * (a3 + a4 * x^1 + a5 * x^2)
x^6 * (a6 + a7 * x^1 + a8 * x^2)
|#

(: powers (bigfloat Exact-Positive-Integer -> (Vectorof bigfloat)))
(define (powers x j)
  (def res (make-vector j (bf 1)))
  (vector-set! res 1 x)
  (for ([i  (in-range 2 j)])
    (vector-set! res i (bf* x (vector-ref res (sub1 i)))))
  res)

(: taylor-sum/smith (bigfloat -> bigfloat))
(define (taylor-sum/smith x)
  (def N (bfexp-terms-ub x (max 1 (- (bflog2-ulp x)))))
  ; abs to keep TR happy
  (def m (abs (fl->exact-integer (flfloor (flsqrt (->fl N))))))
  (def n (ceiling (/ N m)))
  ;(printf "N = ~v, m×n = ~v×~v~n" N m n)
  (def xs (powers x (add1 m)))
  (def x^m (vector-ref xs m))
  
  (: row-sum (Integer Integer -> (values bigfloat Integer)))
  (define (row-sum denom term-num)
    (define-syntax-rule (q j) (if (zero? j) denom (+ term-num j)))
    (define-syntax-rule (a j) (vector-ref xs j))
    (def (values numer new-denom)
      (binary-split/qa* bigfloat Integer bf+ bf* * bf*i q a m))
    (values (bf/ numer (bf new-denom))
            (* new-denom (+ term-num m))))
  
  (def (values res denom) (row-sum 1 0))
  (let loop ([i  (sub1 n)] [res res] [denom denom] [term-num m] [y  x^m])
    (cond [(zero? i)  res]
          [else
           (def (values row-res new-denom) (row-sum denom term-num))
           (loop (sub1 i) (bf+ res (bf* y row-res))
                 new-denom (+ term-num m) (bf* y x^m))])))

(: bfexp* (bigfloat -> bigfloat))
(define (bfexp* z)
  (def log-2 (bf-log-2))
  ; range reduction to < 1 using exp(z) = exp(z - n*log(2)) * 2^n
  (def n (bigfloat->integer (bf/ z log-2)))
  (def y (bf- z (bf* (bf n) log-2)))
  ; range reduction using exp(y) = exp(y/2^k)^(2^k)
  (def k (quotient (integer-sqrt (bf-bits)) 2))
  (def x (y . bf/2^ . k))
  ;(printf "n = ~v, y = ~v~n" n y)
  ;(printf "k = ~v, x = ~v~n" k x)
  ; one bit for each sqr
  (parameterize ([bf-bits  (+ (bf-bits) k)])
    ((sqr* (taylor-sum/smith (bfnormalize x)) k) . bf*2^ . n)))

(: bfexp (bigfloat -> bigfloat))
(define (bfexp z)
  (cond [(bfnegative? z)
         (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits))
                          (abs (bfceiling-log2 (bfneg z))))
           (bfinv (bfexp* (bfneg z))))]
        [(bfpositive? z)
         (with-bf-bits (+ 10 (max 16 (bf-bits)) (ceiling-log2i (bf-bits))
                          (abs (bfceiling-log2 z)))
           (bfexp* z))]
        [else  (bf 1)]))

(define-bf-constant bf-e (bfexp (bf 1)))
