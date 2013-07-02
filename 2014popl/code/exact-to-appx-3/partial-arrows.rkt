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

(: bottom/bot (All (X) (Bot-Arrow X Nothing)))
(define (bottom/bot x)
  (((inst const/bot X Bottom) bottom) x))

(: bottom/map (All (X Y) (Map-Arrow X Y)))
(define (bottom/map A)
  (((inst lift/map X Y) bottom/bot) A))

(: bottom/pre (All (X Y) (Pre-Arrow X Y)))
(define (bottom/pre A)
  (((inst lift/pre X Y) bottom/map) A))

(: branches-ref/bot (Tree-Index -> (Bot-Arrow Branches (U Boolean Bottom))))
(define ((branches-ref/bot i) b)
  (branches-ref b i))

(: branches-ref/map (Tree-Index -> (Map-Arrow Branches Boolean)))
(define (branches-ref/map i)
  (lift/map (branches-ref/bot i)))

(: branches-ref/pre (Tree-Index -> (Pre-Arrow Branches Boolean)))
(define (branches-ref/pre i)
  (lift/pre (branches-ref/map i)))

;; ===================================================================================================
;; Partial arrow transformer

(define-syntax-rule (define-transformed-arrow
                      In-Arrow arr/in >>>/in pair/in if/in bottom/in branches-ref/in
                      Out-Arrow arr/out >>>/out pair/out if/out)
  (begin
    (define-type (Out-Arrow X Y) (Tree-Index -> (In-Arrow (Pair Branches X) Y)))
    
    (: fst/in (All (X Y) (In-Arrow (Pair X Y) X)))
    (define (fst/in xy)
      (((inst arr/in (Pair X Y) X) car) xy))
    
    (: snd/in (All (X Y) (In-Arrow (Pair X Y) Y)))
    (define (snd/in xy)
      (((inst arr/in (Pair X Y) Y) cdr) xy))
    
    (: arr/out (All (X Y) ((X -> Y) -> (Out-Arrow X Y))))
    (define (arr/out f)
      (let ([f  ((inst arr/in X Y) f)])
        (λ (idx) (>>>/in (inst snd/in Branches X) f))))
    
    (: >>>/out (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow Y Z) -> (Out-Arrow X Z))))
    (define ((>>>/out f1 f2) idx)
      (let ([f1  (f1 (cons 0 idx))]
            [f2  (f2 (cons 1 idx))])
        (>>>/in (pair/in (inst fst/in Branches X) f1) f2)))
    
    (: pair/out (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow X Z) -> (Out-Arrow X (Pair Y Z)))))
    (define ((pair/out f1 f2) idx)
      (let ([f1  (f1 (cons 0 idx))]
            [f2  (f2 (cons 1 idx))])
        (pair/in f1 f2)))
    
    (: if/out (All (X Y Z) ((Out-Arrow X Boolean) (-> (Out-Arrow X Y)) (-> (Out-Arrow X Y))
                                                  -> (Out-Arrow X Y))))
    (define ((if/out f1 f2 f3) idx)
      (let ([f1  (f1 (cons 0 idx))]
            [f2  (λ () ((f2) (list* 0 1 idx)))]
            [f3  (λ () ((f3) (list* 1 1 idx)))])
        (define b (>>>/in (inst fst/in Branches X) (branches-ref/in idx)))
        (define bot (λ () bottom/in))
        (if/in f1
               (λ () (if/in b f2 bot))
               (λ () (if/in b bot f3)))))
    ))

;; ===================================================================================================
;; Partial bottom arrow

(define-transformed-arrow
  Bot-Arrow arr/bot >>>/bot pair/bot if/bot bottom/bot branches-ref/bot
  PBot-Arrow arr/pbot >>>/pbot pair/pbot if/pbot)

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
  Map-Arrow arr/map >>>/map pair/map if/map bottom/map branches-ref/map
  PMap-Arrow arr/pmap >>>/pmap pair/pmap if/pmap)

(: lift/pmap (All (X Y) ((PBot-Arrow X Y) -> (PMap-Arrow X Y))))
(define ((lift/pmap f) idx)
  (lift/map (f idx)))

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
  Pre-Arrow arr/pre >>>/pre pair/pre if/pre bottom/pre branches-ref/pre
  PPre-Arrow arr/ppre >>>/ppre pair/ppre if/ppre)

(: lift/ppre (All (X Y) ((PMap-Arrow X Y) -> (PPre-Arrow X Y))))
(define ((lift/ppre f) idx)
  (lift/pre (f idx)))

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
