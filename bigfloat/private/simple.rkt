#lang typed/racket/base

(require "utils.rkt" "bf.rkt")

(provide (all-defined-out))

(: bfceiling-log2 (bigfloat -> Integer))
(define (bfceiling-log2 x)
  (def (bigfloat x-sig x-exp) x)
  (when (not (positive? x-sig))
    (raise-type-error 'bfceiling-log2 "bigfloat > 0" (bigfloat x-sig x-exp)))
  (+ (integer-length (sub1 x-sig)) x-exp))

(: bffloor-log2 (bigfloat -> Integer))
(define (bffloor-log2 x)
  (def (bigfloat x-sig x-exp) x)
  (when (not (positive? x-sig))
    (raise-type-error 'bffloor-log2 "bigfloat > 0" (bigfloat x-sig x-exp)))
  (+ (max 0 (sub1 (integer-length x-sig))) x-exp))

(: bf2^ (Integer -> bigfloat))
(define (bf2^ k)
  (new-bigfloat 1 k))

(: bf*2 (bigfloat -> bigfloat))
(define (bf*2 x)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat x-sig (add1 x-exp)))

(: bf/2 (bigfloat -> bigfloat))
(define (bf/2 x)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat x-sig (sub1 x-exp)))

(: bf*4 (bigfloat -> bigfloat))
(define (bf*4 x)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat x-sig (+ x-exp 2)))

(: bf/4 (bigfloat -> bigfloat))
(define (bf/4 x)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat x-sig (- x-exp 2)))

(: bf*2^ (bigfloat Integer -> bigfloat))
(define (bf*2^ x k)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat x-sig (+ x-exp k)))

(: bf/2^ (bigfloat Integer -> bigfloat))
(define (bf/2^ x k)
  (def (bigfloat x-sig x-exp) x)
  (new-bigfloat x-sig (- x-exp k)))

(: bfnext (case-lambda (bigfloat -> bigfloat) (bigfloat Integer -> bigfloat)))
(define bfnext
  (case-lambda:
   [([x : bigfloat])  (bfnext x 1)]
   [([x : bigfloat] [n : Integer])
    (def (bigfloat x-sig x-exp) x)
    (bigfloat (+ x-sig n) x-exp)]))

(: bfprev (case-lambda (bigfloat -> bigfloat) (bigfloat Integer -> bigfloat)))
(define bfprev
  (case-lambda:
   [([x : bigfloat])  (bfprev x 1)]
   [([x : bigfloat] [n : Integer])
    (def (bigfloat x-sig x-exp) x)
    (bigfloat (- x-sig n) x-exp)]))

(: bfmax (bigfloat bigfloat * -> bigfloat))
(define (bfmax x . xs)
  (let loop ([xs xs] [best x])
    (if (null? xs)
        best
        (if ((car xs) . bf> . best)
            (loop (cdr xs) (car xs))
            (loop (cdr xs) best)))))

(: bfmin (bigfloat bigfloat * -> bigfloat))
(define (bfmin x . xs)
  (let loop ([xs xs] [best x])
    (if (null? xs)
        best
        (if ((car xs) . bf< . best)
            (loop (cdr xs) (car xs))
            (loop (cdr xs) best)))))
