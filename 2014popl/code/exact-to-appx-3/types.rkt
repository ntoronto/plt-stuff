#lang typed/racket

(provide (all-defined-out))

(define bottom '⊥)
(define-type Bottom '⊥)
(define-predicate bottom? Bottom)
