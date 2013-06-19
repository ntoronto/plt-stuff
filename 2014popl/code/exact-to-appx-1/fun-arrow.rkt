#lang typed/racket

(provide (all-defined-out))

;; ===================================================================================================
;; Function arrow

(define-type (Fun-Arrow X Y) (X -> Y))

(: fun-arr (All (X Y) ((Fun-Arrow X Y) -> (Fun-Arrow X Y))))
(define (fun-arr f) f)

(: fun->>> (All (X Y Z) ((Fun-Arrow X Y) (Fun-Arrow Y Z) -> (Fun-Arrow X Z))))
(define ((fun->>> f1 f2) x)
  (f2 (f1 x)))

(: fun-pair (All (X Y Z) ((Fun-Arrow X Y) (Fun-Arrow X Z) -> (Fun-Arrow X (Pair Y Z)))))
(define ((fun-pair f1 f2) x)
  (cons (f1 x) (f2 x)))

(: fun-if (All (X Y) ((Fun-Arrow X Boolean) (Fun-Arrow X Y) (Fun-Arrow X Y) -> (Fun-Arrow X Y))))
(define ((fun-if c t f) x)
  (if (c x) (t x) (f x)))

(: fun-id (All (X) (Fun-Arrow X X)))
(define (fun-id x) x)

(: fun-const (All (X Y) (Y -> (Fun-Arrow X Y))))
(define ((fun-const y) x) y)

(: fun-fst (All (X Y) (Fun-Arrow (Pair X Y) X)))
(define fun-fst car)

(: fun-snd (All (X Y) (Fun-Arrow (Pair X Y) Y)))
(define fun-snd cdr)
