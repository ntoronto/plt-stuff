#lang typed/racket

(require "../set-ops.rkt"
         "types.rkt"
         "map-arrow.rkt"
         "branches-monadic.rkt")

(define map-branches-first ((inst map-arr branches (U Boolean Bottom)) branches-first))
(define map-branches-rest ((inst map-arr branches (U branches Bottom)) branches-rest))

;; ===================================================================================================
;; Partial mapping arrow, using monadic state for branches

(define-type (PMap-Arrow X Y) (Map-Arrow (Pair branches X) (Pair branches Y)))

(: pmap-arr (All (X Y) ((X -> Y) -> (PMap-Arrow X Y))))
(define (pmap-arr f)
  (map-arr (λ: ([bx : (Pair branches X)])
             (cons (car bx) (f (cdr bx))))))

(: pmap->>> (All (X Y Z) ((PMap-Arrow X Y) (PMap-Arrow Y Z) -> (PMap-Arrow X Z))))
(define (pmap->>> f1 f2)
  (map->>> f1 f2))

(: pmap-pair (All (X Y Z) ((PMap-Arrow X Y) (PMap-Arrow X Z) -> (PMap-Arrow X (Pair Y Z)))))
(define (pmap-pair f1 f2)
  (map->>>
   (map->>>
    (map-pair (inst map-snd branches X) f1)
    (map-pair (map->>> (inst map-snd X (Pair branches Y))
                       (inst map-snd branches Y))
              (map->>> (map-pair (map->>> (inst map-snd X (Pair branches Y))
                                          (inst map-fst branches Y))
                                 (inst map-fst X (Pair branches Y)))
                       f2)))
   (map-pair (map->>> (inst map-snd Y (Pair branches Z))
                      (inst map-fst branches Z))
             (map-pair (inst map-fst Y (Pair branches Z))
                       (map->>> (inst map-snd Y (Pair branches Z))
                                (inst map-snd branches Z))))))

(: pmap-if (All (X Y) ((PMap-Arrow X Boolean) (PMap-Arrow X Y) (PMap-Arrow X Y) -> (PMap-Arrow X Y))))
(define (pmap-if c t f)
  (let* ([γ  (map-pair (inst map-snd branches X) c)]
         [bs  (map->>> (inst map-snd X (Pair branches Boolean))
                       (inst map-fst branches Boolean))]
         [b   (map->>> bs map-branches-first)]
         [bs  (map->>> bs map-branches-rest)]
         [c  (map->>> (inst map-snd X (Pair branches Boolean))
                      (inst map-snd branches Boolean))]
         [x  (inst map-fst X (Pair branches Boolean))]
         [t  (map->>> (map-pair bs x) t)]
         [f  (map->>> (map-pair bs x) f)])
    (map->>>
     γ
     (map-if c
             (map-if b t ((inst map-const (Pair X (Pair branches Boolean)) Bottom) bottom))
             (map-if b ((inst map-const (Pair X (Pair branches Boolean)) Bottom) bottom) f)))))

(: pmap-delay (All (X Y) ((-> (PMap-Arrow X Y)) -> (PMap-Arrow X Y))))
(define (pmap-delay f) (map-delay f))

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
           (pmap-delay (λ () ((inst pmap-const Boolean Boolean) #t)))
           (pmap-delay (λ () pmap-halt-on-true))))

;; Doesn't diverge:
(pmap-halt-on-true (set-product some-branches (set #t #f)))
