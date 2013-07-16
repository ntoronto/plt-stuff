#lang typed/racket

(require "../set-ops.rkt"
         "types.rkt"
         "mapping.rkt"
         "preimage.rkt"
         "bot-arrow.rkt"
         "map-arrow.rkt"
         "pre-arrow.rkt"
         "branches.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; More lifts

(: π/bot (Tree-Index -> (Bot-Arrow Branches Boolean)))
(define ((π/bot j) b)
  (define x ((π j) b))
  (if (bottom? x) bottom (just x)))

(: π/map (Tree-Index -> (Map-Arrow Branches Boolean)))
(define (π/map j)
  (lift/map (π/bot j)))

(: π/pre (Tree-Index -> (Pre-Arrow Branches Boolean)))
(define (π/pre j)
  (lift/pre (π/map j)))

;; ===================================================================================================
;; Partial arrow transformer

(define-syntax-rule (define-transformed-arrow
                      In-Arrow arr/in >>>/in pair/in if/in lazy/in error/in assert=/in π/in
                      Out-Arrow arr/out >>>/out pair/out if/out lazy/out)
  (begin
    (define-type (Out-Arrow X Y) (Tree-Index -> (In-Arrow (Pair Branches X) Y)))
    
    (: fst/in (All (X Y) (In-Arrow (Pair X Y) X)))
    (define (fst/in xy)
      (((inst arr/in (Pair X Y) X) car) xy))
    
    (: snd/in (All (X Y) (In-Arrow (Pair X Y) Y)))
    (define (snd/in xy)
      (((inst arr/in (Pair X Y) Y) cdr) xy))
    
    (: lift/out (All (X Y) ((In-Arrow X Y) -> (Out-Arrow X Y))))
    (define ((lift/out f) j)
      (>>>/in (inst snd/in Branches X) f))
    
    (: arr/out (All (X Y) ((X -> Y) -> (Out-Arrow X Y))))
    (define (arr/out f)
      (lift/out ((inst arr/in X Y) f)))
    
    (: >>>/out (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow Y Z) -> (Out-Arrow X Z))))
    (define ((>>>/out f1 f2) j)
      (>>>/in (pair/in (inst fst/in Branches X) (f1 (left j))) (f2 (right j))))
    
    (: pair/out (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow X Z) -> (Out-Arrow X (Pair Y Z)))))
    (define ((pair/out f1 f2) j)
      (pair/in (f1 (left j)) (f2 (right j))))
    
    (: lazy*/out (All (X Y) ((-> (Out-Arrow X Y)) -> (Out-Arrow X Y))))
    (define ((lazy*/out f) j) (lazy/in (λ () ((f) j))))
    
    (: if/out (All (X Y) ((Out-Arrow X Boolean) (Out-Arrow X Y) (Out-Arrow X Y) -> (Out-Arrow X Y))))
    (define ((if/out f1 f2 f3) j)
      (if/in (f1 (left j)) (f2 (left (right j))) (f3 (right (right j)))))
    
    (: branch/out (All (X) (Out-Arrow X Boolean)))
    (define (branch/out j)
      (>>>/in (inst fst/in Branches X) (π/in j)))
    
    (: lazy/out (All (X Y) ((-> (Out-Arrow X Y)) -> (Out-Arrow X Y))))
    (define (lazy/out f)
      (lazy*/out (λ () (if/out (inst branch/out X) (f) ((inst lift/out X Y) error/in)))))
    #;
    (define ((lazy/out f) j)
      (lazy/in (λ ()
                 (if/in ((inst branch/out X) j)
                        ((f) j)
                        error/in))))
    
    (: if*/out (All (X Y) ((Out-Arrow X Boolean) (Out-Arrow X Y) (Out-Arrow X Y) -> (Out-Arrow X Y))))
    (define ((if*/out f1 f2 f3) j)
      (if/in (>>>/in (pair/in (f1 (left j)) ((inst branch/out X) j)) (inst assert=/in Boolean))
             (f2 (left (right j)))
             (f3 (right (right j)))))
    ))

;; ===================================================================================================
;; Partial bottom arrow

(define-transformed-arrow
  Bot-Arrow arr/bot >>>/bot pair/bot if/bot lazy/bot error/bot assert=/bot π/bot
  PBot-Arrow arr/pbot >>>/pbot pair/pbot if/pbot lazy/pbot)

(: ap/pbot (All (X Y) ((PBot-Arrow X Y) X -> (U Bottom (just Y)))))
(define (ap/pbot f x)
  (define B (set-image (f j0) (set-product some-branches (set x))))
  (define C ((inst set-filter-out (just Y) Bottom) bottom? B))
  (if (set-empty? C) bottom (set-take C)))

(: id/pbot (All (X) (PBot-Arrow X X)))
(define (id/pbot x)
  (((inst arr/pbot X X) (λ (x) x)) x))

(: const/pbot (All (X Y) (Y -> (PBot-Arrow X Y))))
(define (const/pbot y)
  ((inst arr/pbot X Y) (λ (x) y)))

(: fst/pbot (All (X Y) (PBot-Arrow (Pair X Y) X)))
(define (fst/pbot xy)
  (((inst arr/pbot (Pair X Y) X) car) xy))

(: snd/pbot (All (X Y) (PBot-Arrow (Pair X Y) Y)))
(define (snd/pbot xy)
  (((inst arr/pbot (Pair X Y) Y) cdr) xy))

;; ===================================================================================================
;; Partial mapping arrow

(define-transformed-arrow
  Map-Arrow arr/map >>>/map pair/map if/map lazy/map error/map assert=/map π/map
  PMap-Arrow arr/pmap >>>/pmap pair/pmap if/pmap lazy/pmap)

(: lift/pmap (All (X Y) ((PBot-Arrow X Y) -> (PMap-Arrow X Y))))
(define ((lift/pmap f) j)
  (lift/map (f j)))

(: id/pmap (All (X) (PMap-Arrow X X)))
(define (id/pmap x)
  (((inst arr/pmap X X) (λ (x) x)) x))

(: const/pmap (All (X Y) (Y -> (PMap-Arrow X Y))))
(define (const/pmap y)
  ((inst arr/pmap X Y) (λ (x) y)))

(: fst/pmap (All (X Y) (PMap-Arrow (Pair X Y) X)))
(define (fst/pmap xy)
  (((inst arr/pmap (Pair X Y) X) car) xy))

(: snd/pmap (All (X Y) (PMap-Arrow (Pair X Y) Y)))
(define (snd/pmap xy)
  (((inst arr/pmap (Pair X Y) Y) cdr) xy))

;; ===================================================================================================
;; Partial preimage arrow

(define-transformed-arrow
  Pre-Arrow arr/pre >>>/pre pair/pre if/pre lazy/pre error/pre assert=/pre π/pre
  PPre-Arrow arr/ppre >>>/ppre pair/ppre if/ppre lazy/ppre)

(: lift/ppre (All (X Y) ((PMap-Arrow X Y) -> (PPre-Arrow X Y))))
(define ((lift/ppre f) j)
  (lift/pre (f j)))

(: id/ppre (All (X) (PPre-Arrow X X)))
(define (id/ppre x)
  (((inst arr/ppre X X) (λ (x) x)) x))

(: const/ppre (All (X Y) (Y -> (PPre-Arrow X Y))))
(define (const/ppre y)
  ((inst arr/ppre X Y) (λ (x) y)))

(: fst/ppre (All (X Y) (PPre-Arrow (Pair X Y) X)))
(define (fst/ppre xy)
  (((inst arr/ppre (Pair X Y) X) car) xy))

(: snd/ppre (All (X Y) (PPre-Arrow (Pair X Y) Y)))
(define (snd/ppre xy)
  (((inst arr/ppre (Pair X Y) Y) cdr) xy))
