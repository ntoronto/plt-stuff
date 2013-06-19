#lang typed/racket

(require "../set-ops.rkt"
         "types.rkt"
         "mapping.rkt"
         "fun-arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Mapping arrow

(define-type (Map-Arrow X Y) ((Setof X) -> (Mapping X Y)))

(: map-arr (All (X Y) ((Fun-Arrow X Y) -> (Map-Arrow X Y))))
(define ((map-arr f) A)
  (set-filter-out
   bottom?
   (set-image (λ: ([x : X])
                (define y (f x))
                (if (bottom? y) bottom (cons x y)))
              A)))

(: map->>> (All (X Y Z) ((Map-Arrow X Y) (Map-Arrow Y Z) -> (Map-Arrow X Z))))
(define ((map->>> f1 f2) A)
  (let* ([f1  (map-run f1 A)]
         [f2  (map-run f2 (mapping-range f1))])
    (mapping (λ: ([x : X]) (mapping-ap f2 (mapping-ap f1 x)))
             (mapping-preimage f1 (mapping-domain f2))
             #;
             (fun->>> (λ (x) (mapping-ap f1 x))
                      (λ (y) (mapping-ap f2 y)))
             #;
             (mapping-domain f1)
             
             )))

(: map-pair (All (X Y Z) ((Map-Arrow X Y) (Map-Arrow X Z) -> (Map-Arrow X (Pair Y Z)))))
(define ((map-pair f1 f2) A)
  (let* ([f1  (map-run f1 A)]
         [f2  (map-run f2 A)])
    (mapping (λ: ([x : X])
               (cons (mapping-ap f1 x) (mapping-ap f2 x)))
             #;
             (fun-pair (λ (x) (mapping-ap f1 x))
                       (λ (x) (mapping-ap f2 x)))
             (set-intersect (mapping-domain f1)
                            (mapping-domain f2)))))

(: map-lazy (All (X Y) ((-> (Map-Arrow X Y)) -> (Map-Arrow X Y))))
(define ((map-lazy f) A)
  ((f) A))

(: map-run (All (X Y) ((Map-Arrow X Y) (Setof X) -> (Mapping X Y))))
(define (map-run f A)
  (if (set-empty? A) (set) (f A)))

(: map-if (All (X Y) ((Map-Arrow X Boolean) (-> (Map-Arrow X Y)) (-> (Map-Arrow X Y)) -> (Map-Arrow X Y))))
(define ((map-if c t f) A)
  (let* ([c  (c A)]
         [t  (map-run (t) (mapping-preimage c (set #t)))]
         [f  (map-run (f) (mapping-preimage c (set #f)))])
    (mapping (λ: ([x : X])
               (if (mapping-ap c x)
                   (mapping-ap t x)
                   (mapping-ap f x)))
             #;
             (fun-if (λ (x) (mapping-ap c x))
                     (λ (x) (mapping-ap t x))
                     (λ (x) (mapping-ap f x)))
             (set-union (mapping-domain t) (mapping-domain f)))))

(: map-id (All (X) (Map-Arrow X X)))
(define (map-id A)
  (((inst map-arr X X) (λ: ([x : X]) x)) A))

(: map-const (All (X Y) (Y -> (Map-Arrow X Y))))
(define ((map-const y) A)
  (((inst map-arr X Y) (λ: ([x : X]) y)) A))

(: map-fst (All (X Y) (Map-Arrow (Pair X Y) X)))
(define (map-fst A)
  (((inst map-arr (Pair X Y) X) car) A))

(: map-snd (All (X Y) (Map-Arrow (Pair X Y) Y)))
(define (map-snd A)
  (((inst map-arr (Pair X Y) Y) cdr) A))

(: map-bottom (All (X) (Map-Arrow X Nothing)))
(define (map-bottom A)
  (((inst map-arr X Nothing) fun-bottom) A))

;; ===================================================================================================

(: map-halt-on-true (Map-Arrow Boolean Boolean))
(define map-halt-on-true
  (map-if (inst map-id Boolean)
          (λ () ((inst map-const Boolean Boolean) #t))
          (λ () map-halt-on-true)))

(map-halt-on-true (set #t))
;; Diverges:
;(map-halt-on-true (set #f))

;; ===================================================================================================

(: apply (All (X Y) ((Pair (X -> Y) X) -> Y)))
(define (apply fx)
  ((car fx) (cdr fx)))

(: map-first (All (X Y Z) ((Map-Arrow X Y) -> (Map-Arrow (Pair X Z) (Pair Y Z)))))
(define (map-first f)
  (map-pair (map->>> (inst map-fst X Z) f) (inst map-snd X Z)))

;; ---------------------------------------------------------------------------------------------------
;; Turning the arrow into an idiom:

; (A ~> B) -> (1 ~> (A -> B))
(: map-delay (All (X Y) ((Map-Arrow X Y) -> (Map-Arrow 0 (X -> Y)))))
(define ((map-delay f) Z)
  (mapping (λ: ([z : (U 0 Bottom)])
             (λ: ([x : X])
               (define y (mapping-ap (f (set x)) x))
               (if (bottom? y) (error 'bottom) y)))
           Z))

; (1 ~> (A -> B)) -> (A ~> B)
(: map-force (All (X Y) ((Map-Arrow 0 (X -> Y)) -> (Map-Arrow X Y))))
(define (map-force f)
  (map->>> (map->>> ((inst map-arr X (Pair 0 X)) (λ: ([x : X]) (cons 0 x)))
                    ((inst map-first 0 (X -> Y) X) f))
           ((inst map-arr (Pair (X -> Y) X) Y) apply)))

;; Objection: don't want to lose expressive power
;; Objection: sets of functions

;; ---------------------------------------------------------------------------------------------------
;; Turning the arrow into a monad

; (A ~> B)×A ~> B
(: map-app (All (X Y) (Map-Arrow (Pair (Map-Arrow X Y) X) Y)))
(define (map-app A)
  (error 'undefined))

;; Objection: sets of functions
