#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bfagm.rkt" "bf-pi.rkt")

(require/typed
 racket/base
 [integer-sqrt  (Natural -> Natural)])

(provide bflog bf-log-2 bf-log-10)

(: jtheta2/z=0 (bigfloat -> bigfloat))
;; Calculates elliptic theta_2 specialized to z=0
(define (jtheta2/z=0 q)
  (def q^2 (bfsqr q))
  (def one (bf 1))
  (def s (let: loop : bigfloat ([s : bigfloat  one]
                                [q^nn+n : bigfloat  one]
                                [q^2n : bigfloat  q^2])
           (def new-q^nn+n (bf* q^nn+n q^2n))
           (def new-s (bf+ s new-q^nn+n))
           (if (bf= s new-s) s (loop new-s new-q^nn+n (bf* q^2n q^2)))))
  (bf* (bfsqrt (bfsqrt q)) (s . bf*2^ . 1)))

(: jtheta3/z=0 (bigfloat -> bigfloat))
;; Calculates elliptic theta_3 specialized to z=0
(define (jtheta3/z=0 q)
  (def q^2 (bfsqr q))
  (def s (let: loop : bigfloat ([s : bigfloat  q]
                                [q^n^2 : bigfloat  q]
                                [q^2n+1 : bigfloat  (bf* q^2 q)])
           (def new-q^n^2 (bf* q^n^2 q^2n+1))
           (def new-s (bf+ s new-q^n^2))
           (if (bf= s new-s) s (loop new-s new-q^n^2 (bf* q^2n+1 q^2)))))
  (bf+ (bf 1) (s . bf*2^ . 1)))

(: bflog** (bigfloat -> bigfloat))
(define (bflog** x)
  (def a (bfsqr (jtheta2/z=0 x)))
  (def b (bfsqr (jtheta3/z=0 x)))
  (def pi (bf-pi))
  (def agm-a-b (bfagm a b))
  (bfneg (bf/ pi agm-a-b)))

(define-bf-constant bf-log-2 (bfneg (bflog** (bf 1/2))))
(define-bf-constant bf-log-10 (bfneg (bflog** (bf 1/10))))

(: bflog* (bigfloat -> bigfloat))
(define (bflog* x)
  ; reduce using log(x) = k*log(2) + log(x/2^k)
  (def k (integer-sqrt (bf-bits)))
  (parameterize ([bf-bits  (+ (bf-bits) k)])
    (def (values z s)
      (if (x . bf> . (bigfloat 1 (- k)))
          (values (x . bf/2^ . k) (bf*i (bf-log-2) k))
          (values x (bf 0))))
    (bf+ s (bflog** z))))

(: bflog (bigfloat -> bigfloat))
(define (bflog x)
  (cond [(not (bfpositive? x))  (raise-type-error 'bflog "bigfloat > 0" x)]
        [(bf= x (bf 1))      (bf 0)]
        [(bf= x (bf 2))      (bf-log-2)]
        [(bf= x (bf 10))     (bf-log-10)]
        [(x . bf> . (bf 1))  (with-bf-bits (+ 10 (max 16 (bf-bits))
                                              (ceiling-log2i (bf-bits)))
                               (bfneg (bflog* (bfinv x))))]
        [(x . bf< . (bf 1))  (with-bf-bits (+ 10 (max 16 (bf-bits))
                                              (ceiling-log2i (bf-bits)))
                               (bflog* x))]
        [else  (bf 0)]))
