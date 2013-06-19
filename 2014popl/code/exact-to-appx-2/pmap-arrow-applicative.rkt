#lang typed/racket

(require "../set-ops.rkt"
         "types.rkt"
         "map-arrow.rkt"
         "branches-applicative.rkt")

(provide (all-defined-out))

(: map-branches-ref (Tree-Index -> (Map-Arrow Branches Boolean)))
(define (map-branches-ref i)
  (map-arr (λ: ([b : Branches]) (branches-ref b i))))

;; ===================================================================================================
;; Partial mapping arrow, using expression indexes and applicative state for branches

(define-type (PMap-Arrow X Y) (Tree-Index -> (Map-Arrow (Pair Branches X) Y)))

(: pmap-arr (All (X Y) ((X -> Y) -> (PMap-Arrow X Y))))
(define ((pmap-arr f) idx)
  (map-arr (λ: ([bx : (Pair Branches X)])
             (f (cdr bx)))))

(: pmap->>> (All (X Y Z) ((PMap-Arrow X Y) (PMap-Arrow Y Z) -> (PMap-Arrow X Z))))
(define ((pmap->>> f1 f2) idx)
  (let ([f1  (f1 (cons 0 idx))]
        [f2  (f2 (cons 1 idx))])
    (map->>> (map-pair (inst map-fst Branches X) f1) f2)))

(: pmap-pair (All (X Y Z) ((PMap-Arrow X Y) (PMap-Arrow X Z) -> (PMap-Arrow X (Pair Y Z)))))
(define ((pmap-pair f1 f2) idx)
  (let ([f1  (f1 (cons 0 idx))]
        [f2  (f2 (cons 1 idx))])
    (map-pair f1 f2)))

(: pmap-if (All (X Y) ((PMap-Arrow X Boolean) (PMap-Arrow X Y) (PMap-Arrow X Y) -> (PMap-Arrow X Y))))
(define ((pmap-if c t f) idx)
  (let ([c  (c (cons 0 idx))]
        [t  (t (list* 0 1 idx))]
        [f  (f (list* 1 1 idx))])
    (define b (map->>> (inst map-fst Branches X) (map-branches-ref idx)))
    (map-if c
            (map-if b t map-bottom)
            (map-if b map-bottom f))))

(: pmap-lazy (All (X Y) ((-> (PMap-Arrow X Y)) -> (PMap-Arrow X Y))))
(define ((pmap-lazy f) idx)
  (map-lazy (λ () ((f) idx))))

(: pmap-id (All (X) (PMap-Arrow X X)))
(define (pmap-id A)
  (((inst pmap-arr X X) (λ: ([x : X]) x)) A))

(: pmap-const (All (X Y) (Y -> (PMap-Arrow X Y))))
(define ((pmap-const y) A)
  (((inst pmap-arr X Y) (λ: ([x : X]) y)) A))

(: pmap-fst (All (X Y) (PMap-Arrow (Pair X Y) X)))
(define (pmap-fst A)
  (((inst pmap-arr (Pair X Y) X) car) A))

(: pmap-snd (All (X Y) (PMap-Arrow (Pair X Y) Y)))
(define (pmap-snd A)
  (((inst pmap-arr (Pair X Y) Y) cdr) A))

;; ===================================================================================================

(: pmap-halt-on-true (PMap-Arrow Boolean Boolean))
(define pmap-halt-on-true
  (pmap-if (inst pmap-id Boolean)
           (pmap-lazy (λ () ((inst pmap-const Boolean Boolean) #t)))
           (pmap-lazy (λ () pmap-halt-on-true))))

;; Doesn't diverge:
((pmap-halt-on-true '()) (set-product some-branches (set #t #f)))
