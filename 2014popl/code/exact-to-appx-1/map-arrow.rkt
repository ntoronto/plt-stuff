#lang typed/racket

(require "../set-ops.rkt"
         "mapping.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Mapping arrow

(define-type (Map-Arrow X Y) ((Setof X) -> (Mapping X Y)))

(: map-arr (All (X Y) ((X -> Y) -> (Map-Arrow X Y))))
(define ((map-arr f) A)
  (mapping f A))

(: map->>> (All (X Y Z) ((Map-Arrow X Y) (Map-Arrow Y Z) -> (Map-Arrow X Z))))
(define ((map->>> f1 f2) A)
  (let ([f1  (f1 A)])
    (mapping-compose (f2 (mapping-range f1)) f1)))

(: map-pair (All (X Y Z) ((Map-Arrow X Y) (Map-Arrow X Z) -> (Map-Arrow X (Pair Y Z)))))
(define ((map-pair f1 f2) A)
  (let ([f1  (f1 A)]
        [f2  (f2 A)])
    (mapping (λ: ([x : X]) (cons (mapping-ap f1 x) (mapping-ap f2 x)))
             (set-intersect (mapping-domain f1) (mapping-domain f2)))))

(: map-if (All (X Y) ((Map-Arrow X Boolean) (Map-Arrow X Y) (Map-Arrow X Y) -> (Map-Arrow X Y))))
(define ((map-if c t f) A)
  (let ([c  (c A)])
    (let ([t  (t (mapping-preimage c (set #t)))]
          [f  (f (mapping-preimage c (set #f)))])
      (mapping (λ: ([x : X]) (if (mapping-ap c x) (mapping-ap t x) (mapping-ap f x)))
               (set-union (mapping-domain t) (mapping-domain f))))))

(: map-id (All (X) (Map-Arrow X X)))
(define (map-id A)
  (((inst map-arr X X) (λ: ([x : X]) x)) A))

(: map-const (All (X Y) (Y -> (Map-Arrow X Y))))
(define ((map-const y) A)
  (((inst map-arr X Y) (λ: ([x : X]) y)) A))

(: map-fst (All (X Y) (Map-Arrow (Pair X Y) X)))
(define (map-fst A1×A2)
  (((inst map-arr (Pair X Y) X) car) A1×A2))

(: map-snd (All (X Y) (Map-Arrow (Pair X Y) Y)))
(define (map-snd A1×A2)
  (((inst map-arr (Pair X Y) Y) cdr) A1×A2))
