#lang typed/racket/base

(require "utils.rkt" "bf.rkt" "syntax.rkt" "binary-split.rkt")

(require/typed
 racket/base
 [integer-sqrt  (Natural -> Natural)])

(provide bf-pi)

;; Computed using Ramanujan 163 with binary splitting
(define-bf-constant bf-pi
  (def N (max 1 (numerator (round (* #e0.05247673 (+ (bf-bits) 2))))))
  (def A 163096908)
  (def B 6541681608)
  (def J1 10939058860032000)
  (def J3 262537412640768000)
  (define-syntax-rule (a n) (+ A (* n B)))
  (define-syntax-rule (p n)
    (if (zero? n) 1 (- (* (- (* 6 n) 5) (- (* 2 n) 1) (- (* 6 n) 1)))))
  (define-syntax-rule (q n) (if (zero? n) 1 (* n n n J1)))
  (def (values u v) (binary-split/pqa Integer + * p q a N))
  (bf (/ (/ (integer-sqrt (J3 . << . (*2 (bf-bits))))
            (2^ (bf-bits)))
         (/ u v))))
