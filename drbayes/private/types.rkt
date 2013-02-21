#lang typed/racket/base

(require racket/list
         racket/match)

(provide (all-defined-out))

(define-type Omega-Idx Nonnegative-Exact-Rational)

(: omega-expr-idx (Omega-Idx Omega-Idx -> Omega-Idx))
(define (omega-expr-idx r0 r1)
  (* 1/2 (+ r0 r1)))

(define-type Idx (U Symbol Natural Omega-Idx))
