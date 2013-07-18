#lang typed/racket

(require "../set-ops.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Strict mappings

(define-type (Mapping X Y) (Setof (Pair X Y)))

(: mapping (All (X Y) ((X -> Y) (Setof X) -> (Mapping X Y))))
(define (mapping f A)
  (set-image (λ: ([x : X]) (cons x (f x))) A))

(: mapping-domain (All (X Y) ((Mapping X Y) -> (Setof X))))
(define (mapping-domain f)
  (set-image (inst car X Y) f))

(: mapping-range (All (X Y) ((Mapping X Y) -> (Setof Y))))
(define (mapping-range f)
  (set-image (inst cdr X Y) f))

(: mapping-ap (All (X Y) ((Mapping X Y) X -> Y)))
(define (mapping-ap f x)
  (define A (set-filter (λ: ([xy : (Pair X Y)]) (equal? x (car xy))) f))
  (cond [(set-empty? A)  (error 'bad)]
        [else  (cdr (set-take A))]))

(: mapping-image (All (X Y) ((Mapping X Y) (Setof X) -> (Setof Y))))
(define (mapping-image f A)
  (set-image (λ: ([x : X]) (mapping-ap f x))
             (set-intersect A (mapping-domain f))))

(: mapping-preimage (All (X Y) ((Mapping X Y) (Setof Y) -> (Setof X))))
(define (mapping-preimage f B)
  (set-preimage (λ: ([x : X]) (mapping-ap f x))
                (mapping-domain f)
                B))

(: mapping-compose (All (X Y Z) ((Mapping Y Z) (Mapping X Y) -> (Mapping X Z))))
(define (mapping-compose g2 g1)
  (mapping (λ (x) (mapping-ap g2 (mapping-ap g1 x)))
           (mapping-preimage g1 (mapping-domain g2))))

(: mapping-pair (All (X Y Z) ((Mapping X Y) (Mapping X Z) -> (Mapping X (Pair Y Z)))))
(define (mapping-pair g1 g2)
  (mapping (λ (x) (cons (mapping-ap g1 x) (mapping-ap g2 x)))
           (set-intersect (mapping-domain g1) (mapping-domain g2))))

(: mapping-disjoint-union (All (X Y) ((Mapping X Y) (Mapping X Y) -> (Mapping X Y))))
(define (mapping-disjoint-union g1 g2)
  (define A (set-disjoint-union (mapping-domain g1) (mapping-domain g2)))
  (set-union g1 g2))

(: mapping-restrict (All (X Y) ((Mapping X Y) (Setof X) -> (Mapping X Y))))
(define (mapping-restrict f A)
  (set-filter (λ: ([xy : (Pair X Y)]) (set-member? A (car xy))) f))

(: mapping-range-restrict (All (X Y) ((Mapping X Y) (Setof Y) -> (Mapping X Y))))
(define (mapping-range-restrict f B)
  (set-filter (λ: ([xy : (Pair X Y)]) (set-member? B (cdr xy))) f))

;; ===================================================================================================
;; Lazy mappings
#|
(define-type (Lazy-Mapping-Fun X Y) (X -> Y))

(struct: (X Y) lazy-mapping ([function : (Lazy-Mapping-Fun X Y)]
                             [domain : (Setof X)])
  #:transparent)

(: lazy->mapping (All (X Y) ((lazy-mapping X Y) -> (Mapping X Y))))
(define (lazy->mapping f)
  (mapping (lazy-mapping-function f) (lazy-mapping-domain f)))

(: mapping->lazy (All (X Y) ((Mapping X Y) -> (lazy-mapping X Y))))
(define (mapping->lazy f)
  (lazy-mapping (λ: ([x : X]) (mapping-ap/error f x))
                (mapping-domain f)))

(: lazy-mapping-range (All (X Y) ((lazy-mapping X Y) -> (Setof Y))))
(define (lazy-mapping-range f)
  (set-image (lazy-mapping-function f) (lazy-mapping-domain f)))

(: lazy-mapping-ap (All (X Y) ((lazy-mapping X Y) X -> Y)))
(define (lazy-mapping-ap f x)
  (match-define (lazy-mapping g A) f)
  (cond [(set-member? A x)  (g x)]
        [else  (raise-argument-error 'lazy-mapping-ap "in-domain value" 1 f x)]))

(: lazy-mapping-image (All (X Y) ((lazy-mapping X Y) (Setof X) -> (Setof Y))))
(define (lazy-mapping-image f A)
  (set-image (lazy-mapping-function f)
             (set-intersect A (lazy-mapping-domain f))))

(: lazy-mapping-preimage (All (X Y) ((lazy-mapping X Y) (Setof Y) -> (Setof X))))
(define (lazy-mapping-preimage f B)
  (set-preimage (lazy-mapping-function f)
                (lazy-mapping-domain f)
                B))

(: lazy-mapping-compose (All (X Y Z) ((lazy-mapping Y Z) (lazy-mapping X Y) -> (lazy-mapping X Z))))
(define (lazy-mapping-compose f g)
  (lazy-mapping (λ: ([x : X]) (lazy-mapping-ap f (lazy-mapping-ap g x)))
                (lazy-mapping-domain g)))

(: lazy-mapping-restrict (All (X Y) ((lazy-mapping X Y) (Setof X) -> (lazy-mapping X Y))))
(define (lazy-mapping-restrict f A)
  (lazy-mapping (lazy-mapping-function f)
                (set-intersect A (lazy-mapping-domain f))))
|#
