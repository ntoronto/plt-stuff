#lang typed/racket

;; Defines the various kinds of preimage mappings (strict and lazy) used in step-*.rkt

(require "../set-ops.rkt"
         "../standard-algebra.rkt"
         
         "mapping.rkt"
         )

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage mappings

(define-type (Preimage X Y) (Mapping (Setof Y) (Setof X)))

(: preimage-ap (All (X Y) ((Preimage X Y) (Setof Y) -> (Setof X))))
;; preimage-ap f B  :=  f (B ∩ (∪ (domain f)))
(define (preimage-ap f orig-B)
  (define Cs (mapping-domain f))
  (define C (set-union* Cs))
  (define B (set-intersect orig-B C))
  (cond [(set-member? Cs B)  (mapping-ap/error f B)]
        [(set-empty? B)
         (error 'preimage-ap "internal error: preimage mapping ~e does not map the empty set" f)]
        [else
         (raise-argument-error 'preimage-ap "measurable set" 1 f orig-B)]))

(: preimage-range-algebra (All (X Y) ((Preimage X Y) -> (Setof (Setof Y)))))
(define (preimage-range-algebra f)
  (mapping-domain f))

(: preimage-range (All (X Y) ((Preimage X Y) -> (Setof Y))))
(define (preimage-range f)
  (set-union* (preimage-range-algebra f)))

(: preimage-compose (All (X Y Z) ((Preimage Y Z) (Preimage X Y) -> (Preimage X Z))))
(define (preimage-compose f g)
  (cond [(equal? (mapping-range f) (mapping-domain g))
         (set-image (λ: ([zy : (Pair (Setof Z) (Setof Y))])
                      (define y (cdr zy))
                      (define x ((inst mapping-ap/error (Setof Y) (Setof X)) g y))
                      (cons (car zy) x))
                    f)]
        [else
         (error 'preimage-compose "cannot compose ~e and ~e" f g)]))

(: preimage-close-op (All (X Y) (Binary-Set-Op (Preimage X Y) -> (Preimage X Y))))
;; f ∪ {A ∈ f}. {B ∈ f}. {((fst A) ∪ (fst B), (snd A) ∪ (snd B))}
(define (preimage-close-op op f)
  (set-union f (set-bind f (λ: ([A : (Pair (Setof Y) (Setof X))])
                             (set-bind f (λ: ([B : (Pair (Setof Y) (Setof X))])
                                           (set (cons (op (car A) (car B))
                                                      (op (cdr A) (cdr B))))))))))

(: preimage-close (All (X Y) ((Preimage X Y) -> (Preimage X Y))))
(define (preimage-close f)
  (let* ([g  (preimage-close-op set-union f)]
         [g  (preimage-close-op set-intersect2 g)]
         [g  (preimage-close-op set-complement g)])
    (if (equal? f g) f (preimage-close g))))
#|
;; ===================================================================================================
;; Lazy preimage mappings

(define-type (Lazy-Preimage X Y) (lazy-mapping (Setof Y) (Setof X)))

(: lazy-preimage-ap (All (X Y) ((Lazy-Preimage X Y) (Setof Y) -> (Setof X))))
(define (lazy-preimage-ap f orig-B)
  (define Cs (lazy-mapping-domain f))
  (define C (set-union* Cs))
  (define B (set-intersect orig-B C))
  (cond [(set-member? Cs B)  (lazy-mapping-ap f B)]
        [(set-empty? B)
         (error 'lazy-preimage-ap "internal error: preimage mapping ~e does not map the empty set" f)]
        [else
         (raise-argument-error 'lazy-preimage-ap "measurable set" 1 f orig-B)]))

(: lazy-preimage-close (All (X Y) ((Lazy-Preimage X Y) -> (Lazy-Preimage X Y))))
(define (lazy-preimage-close f)
  (mapping->lazy (preimage-close (lazy->mapping f))))

(: lazy-preimage-range-algebra (All (X Y) ((Lazy-Preimage X Y) -> (Setof (Setof Y)))))
(define (lazy-preimage-range-algebra f)
  (lazy-mapping-domain f))

(: lazy-preimage-range (All (X Y) ((Lazy-Preimage X Y) -> (Setof Y))))
(define (lazy-preimage-range f)
  (set-union* (lazy-preimage-range-algebra f)))

(: lazy-preimage-compose (All (X Y Z) ((Lazy-Preimage Y Z) (Lazy-Preimage X Y)
                                                           -> (Lazy-Preimage X Z))))
(define (lazy-preimage-compose f1 f2)
  (lazy-mapping-compose f2 f1))


;; ===================================================================================================
;; Lazy preimage mappings for standard algebras

(struct: (X Y) std-preimage ([function : ((Setof Y) -> (Setof X))]
                             [range : (Setof Y)])
  #:transparent)

(: std-preimage-ap (All (X Y) ((std-preimage X Y) (Setof Y) -> (Setof X))))
(define (std-preimage-ap f orig-B)
  (define C (std-preimage-range f))
  (define Cs (standard-algebra C))
  (define B (set-intersect orig-B C))
  (cond [(set-member? Cs B)  ((std-preimage-function f) B)]
        [(set-empty? B)
         (error 'std-preimage-ap
                "internal error: preimage mapping ~e does not map the empty set" f)]
        [else
         (raise-argument-error 'std-preimage-ap "measurable set" 1 f orig-B)]))

(: std-preimage-close (All (X Y) ((std-preimage X Y) -> (std-preimage X Y))))
(define (std-preimage-close f) f)

(: std-preimage-range-algebra (All (X Y) ((std-preimage X Y) -> (Setof (Setof Y)))))
(define (std-preimage-range-algebra f)
  (standard-algebra (std-preimage-range f)))

(: std-preimage-compose (All (X Y Z) ((std-preimage Y Z) (std-preimage X Y) -> (std-preimage X Z))))
(define (std-preimage-compose f1 f2)
  (std-preimage (λ: ([C : (Setof Z)])
                  (define B ((std-preimage-function f1) C))
                  (std-preimage-ap f2 B))
                (std-preimage-range f1)))
|#
