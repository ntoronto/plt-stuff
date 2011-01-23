#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "simple.rkt" "arithmetic.rkt"
         "bfrandom.rkt" "bflog.rkt" "bfexp.rkt")

(require/typed
 racket/base
 [opaque Thread-Cell  thread-cell?]
 [make-thread-cell  (Any -> Thread-Cell)]
 [thread-cell-ref  (Thread-Cell -> Any)]
 [thread-cell-set!  (Thread-Cell Any -> Void)])

(provide (except-out
          (all-defined-out)
          ; necessary because of a bug in TR:
          thread-cell? Thread-Cell
          make-thread-cell thread-cell-ref thread-cell-set!))

(: bfrunif (bigfloat bigfloat -> bigfloat))
(define (bfrunif a b)
  (bf+ a (bf* (bf- b a) (bfrandom))))

(: bfrnorm2 (-> (values bigfloat bigfloat)))
;; Polar method (Box-Muller transform)
(define (bfrnorm2)
  (let: loop : (values bigfloat bigfloat) ()
    (def x (bfrandom-signed))
    (def y (bfrandom-signed))
    (def r (bf+ (bfsqr x) (bfsqr y)))
    (if (or (bfzero? r) (r . bf> . (bf 1)))
        (loop)
        (let ([z  (bfsqrt (bf/ (bf* (bf -2) (bflog r)) r))])
          (values (bf* x z) (bf* y z))))))

(: bfrnorm (case-lambda (-> bigfloat) (bigfloat bigfloat -> bigfloat)))
(define bfrnorm
  (let ([next-rnorm  (make-thread-cell #f)])
    (case-lambda:
     [()  (if (thread-cell-ref next-rnorm)
              (let ([rnorm  (thread-cell-ref next-rnorm)])
                (thread-cell-set! next-rnorm #f)
                (cast rnorm bigfloat?))
              (let-values ([(rnorm1 rnorm2)  (bfrnorm2)])
                (thread-cell-set! next-rnorm rnorm1)
                (cast rnorm2 bigfloat?)))]
     [([ctr : bigfloat] [scl : bigfloat])
      (bf+ ctr (bf* scl (bfrnorm)))])))

(: bfrcauchy (case-lambda (-> bigfloat) (bigfloat bigfloat -> bigfloat)))
(define bfrcauchy
  (case-lambda:
   [()  (def (values rnorm1 rnorm2) (bfrnorm2))
        (bf/ rnorm1 rnorm2)]
   [([ctr : bigfloat] [scl : bigfloat])
    (bf+ ctr (bf* scl (bfrcauchy)))]))

(: bfexpected-value ((bigfloat -> bigfloat) (Listof bigfloat) -> bigfloat))
(define (bfexpected-value f xs)
  (bf/ (bfsum (map f xs)) (bf (length xs))))

(: bfmean ((Listof bigfloat) -> bigfloat))
(define (bfmean xs)
  (bf/ (bfsum xs) (bf (length xs))))

(: bfvariance ((Listof bigfloat) -> bigfloat))
(define (bfvariance xs)
  (bf- (bfexpected-value bfsqr xs) (bfsqr (bfmean xs))))

(: bfstddev ((Listof bigfloat) -> bigfloat))
(define (bfstddev xs)
  (bfsqrt (bfvariance xs)))
