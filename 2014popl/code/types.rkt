#lang typed/racket

(require "untyped-utils.rkt")

(provide (all-defined-out))

;; Maybe type

(define-singleton-type Bottom ⊥)

(struct: (X) just ([value : X]) #:transparent)

(define-type (Maybe X) (U Bottom (just X)))

(: unjust (All (X) ((U Bottom (just X)) -> (U Bottom X))))
(define (unjust v)
  (if (⊥? v) ⊥ (just-value v)))

;; Combinators

(: >>> (All (X Y Z) ((X -> Y) (Y -> Z) -> (X -> Z))))
(define ((>>> f1 f2) x)
  (f2 (f1 x)))

(: pair (All (X Y1 Y2) ((X -> Y1) (X -> Y2) -> (X -> (Pair Y1 Y2)))))
(define ((pair f1 f2) x)
  (cons (f1 x) (f2 x)))
