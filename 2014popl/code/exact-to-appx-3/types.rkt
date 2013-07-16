#lang typed/racket

(provide (all-defined-out))

(define bottom '⊥)
(define-type Bottom '⊥)
(define-predicate bottom? Bottom)

(struct: (X) just ([value : X]) #:transparent)

(: unjust (All (X) ((U Bottom (just X)) -> (U Bottom X))))
(define (unjust v)
  (if (bottom? v) bottom (just-value v)))
