#lang typed/racket

(require "../set-ops.rkt"
         "types.rkt"
         "fun-arrow.rkt"
         "map-arrow.rkt"
         "pre-arrow.rkt"
         "branches-applicative.rkt")

(: fun-branches-ref (Tree-Index -> (Fun-Arrow Branches (U Boolean Bottom))))
(define ((fun-branches-ref i) b)
  (branches-ref b i))

(: map-branches-ref (Tree-Index -> (Map-Arrow Branches Boolean)))
(define (map-branches-ref i)
  (map-arr (fun-branches-ref i)))

(: pre-branches-ref (Tree-Index -> (Pre-Arrow Branches Boolean)))
(define (pre-branches-ref i)
  (pre-arr (map-branches-ref i)))

;; ===================================================================================================
;; Partial mapping arrow, using expression indexes and applicative state for branches

(define-type (PPre-Arrow X Y) (Tree-Index -> (Pre-Arrow (Pair Branches X) Y)))

(: ppre-arr (All (X Y) ((Map-Arrow X Y) -> (PPre-Arrow X Y))))
(define ((ppre-arr f) idx)
  (pre-arr (map->>> (inst map-snd Branches X) f)))

(: ppre->>> (All (X Y Z) ((PPre-Arrow X Y) (PPre-Arrow Y Z) -> (PPre-Arrow X Z))))

(define ((ppre->>> f1 f2) idx)
  (let ([f1  (f1 (cons 0 idx))]
        [f2  (f2 (cons 1 idx))])
    (pre->>> (pre-pair (inst pre-fst Branches X) f1) f2)))

(: ppre-pair (All (X Y Z) ((PPre-Arrow X Y) (PPre-Arrow X Z) -> (PPre-Arrow X (Pair Y Z)))))
(define ((ppre-pair f1 f2) idx)
  (let ([f1  (f1 (cons 0 idx))]
        [f2  (f2 (cons 1 idx))])
    (pre-pair f1 f2)))

(: ppre-if (All (X Y) ((PPre-Arrow X Boolean) (PPre-Arrow X Y) (PPre-Arrow X Y) -> (PPre-Arrow X Y))))
(define ((ppre-if c t f) idx)
  (let ([c  (c (cons 0 idx))]
        [t  (t (list* 0 1 idx))]
        [f  (f (list* 1 1 idx))])
    (define b (pre->>> (inst pre-fst Branches X) (pre-branches-ref idx)))
    (pre-if c
            (pre-if b t pre-bottom)
            (pre-if b pre-bottom f))))

(: ppre-delay (All (X Y) ((-> (PPre-Arrow X Y)) -> (PPre-Arrow X Y))))
(define ((ppre-delay f) idx)
  (pre-lazy (λ () ((f) idx))))

(: ppre-id (All (X) (PPre-Arrow X X)))
(define (ppre-id A)
  (((inst ppre-arr X X) map-id) A))

(: ppre-const (All (X Y) (Y -> (PPre-Arrow X Y))))
(define ((ppre-const y) A)
  (((inst ppre-arr X Y) (map-const y)) A))

(: ppre-fst (All (X Y) (PPre-Arrow (Pair X Y) X)))
(define (ppre-fst A)
  (((inst ppre-arr (Pair X Y) X) map-fst) A))

(: ppre-snd (All (X Y) (PPre-Arrow (Pair X Y) Y)))
(define (ppre-snd A)
  (((inst ppre-arr (Pair X Y) Y) map-snd) A))

;; ===================================================================================================

(: ppre-halt-on-true (PPre-Arrow Boolean Boolean))
(define ppre-halt-on-true
  (ppre-if (inst ppre-id Boolean)
           (ppre-delay (λ () ((inst ppre-const Boolean Boolean) #t)))
           (ppre-delay (λ () ppre-halt-on-true))))

;; Doesn't diverge:
((ppre-halt-on-true '()) (set-product some-branches (set #t #f)))
