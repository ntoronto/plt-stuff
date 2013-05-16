#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "../untyped-utils.rkt"
         "indexes.rkt"
         "arrow-common.rkt")

(provide (all-defined-out))

(define check-rand-preimage-arguments? #t)


(define-type Domain-Set (U Empty-Set nonempty-domain-set))

(struct: nonempty-domain-set ([Ω : Omega-Rect]
                              [Z : Branches-Rect]
                              [A : Nonempty-Set])
  #:transparent)

(: domain-set-intersect (Domain-Set Domain-Set -> Domain-Set))
(define (domain-set-intersect K1 K2)
  (match-define (domain-set Ω1 Z1 A1) K1)
  (match-define (domain-set Ω2 Z2 A2) K2)
  (domain-set (omega-rect-intersect Ω1 Ω2)
              (branches-rect-intersect Z1 Z2)
              (set-intersect A1 A2)))

(: domain-set-subseteq? (Domain-Set Domain-Set -> Boolean))
(define (domain-set-subseteq? K1 K2)
  (match-define (domain-set Ω1 Z1 A1) K1)
  (match-define (domain-set Ω2 Z2 A2) K2)
  (and (omega-rect-subseteq? Ω1 Ω2)
       (branches-rect-subseteq? Z1 Z2)
       (set-subseteq? A1 A2)))

(: make-domain-set (Maybe-Omega-Rect Maybe-Branches-Rect Set -> Domain-Set))
(define (make-domain-set Ω Z A)
  (cond [(or (empty-set? Ω) (empty-set? Z) (empty-set? A))  empty-set]
        [else  (nonempty-domain-set Ω Z A)]))

(define-match-expander domain-set
  (λ (stx)
    (syntax-case stx ()
      [(_ Ω Z A)
       (syntax/loc stx
         (or (nonempty-domain-set Ω Z A)
             (and (? empty-set?)
                  (app (λ (_) empty-set) Ω)
                  (app (λ (_) empty-set) Z)
                  (app (λ (_) empty-set) A))
             (and (app (λ: ([_ : Domain-Set]) empty-set) Ω)
                  (app (λ (_) empty-set) Z)
                  (app (λ (_) empty-set) A))))]))
  (make-head-form #'make-domain-set))

(: domain-set-pair (Domain-Set Domain-Set -> Domain-Set))
(define (domain-set-pair K1 K2)
  (match-define (domain-set Ω1 Z1 A1) K1)
  (match-define (domain-set Ω2 Z2 A2) K2)
  (domain-set (omega-rect-intersect Ω1 Ω2)
              (branches-rect-intersect Z1 Z2)
              (set-pair A1 A2)))

(: domain-set-pair-ref (Domain-Set Pair-Index -> Domain-Set))
(define (domain-set-pair-ref K j)
  (match-define (domain-set Ω Z A) K)
  (domain-set Ω Z (set-pair-ref A j)))

;; ===================================================================================================
;; Random expressions

(define-type Rand-Forward-Fun (Omega Branches Value -> Maybe-Value))

;; Necessary property: computations and preimage functions never grow Ω or Z!

(define-type Rand-Preimage-Fun (nonempty-domain-set nonempty-domain-set -> Domain-Set))

;; A computation is a function from a domain and branches to its meaning
(define-type Rand-Preimage (U Empty-Preimage nonempty-rand-preimage))
(define-type Rand-Computation (nonempty-domain-set -> Rand-Preimage))

(struct: nonempty-rand-preimage ([domain : nonempty-domain-set]
                                 [range : nonempty-domain-set]
                                 [fun : Rand-Preimage-Fun])
  #:transparent)

;; An expression is a function from a reversed index to the expression's meaning
(struct: rand-expression ([fun : (Omega-Index -> rand-expression-meaning)])
  #:transparent)

;; Expressions are currently wrapped in a struct so they can be recognized by its predicate (see
;; "language.rkt"), but this may not be necessary in the future

;; An expression means:
;;  1. Lazy indexes of random variables and branch points
;;  2. A forward function
;;  3. A computation, which is used to compute preimages
(struct: rand-expression-meaning ([indexes : Indexes]
                                  [forward : Rand-Forward-Fun]
                                  [computation : Rand-Computation])
  #:transparent)

;; ===================================================================================================
;; Convenience functions
(: run-rand-expression (case-> (rand-expression -> rand-expression-meaning)
                               (rand-expression Omega-Index -> rand-expression-meaning)))
(define (run-rand-expression e [r empty])
  ((rand-expression-fun e) r))

(: run-rand-forward-fun (Rand-Forward-Fun Omega Branches Maybe-Value -> Maybe-Value))
(define (run-rand-forward-fun f ω z γ)
  (cond [(bottom? γ)  γ]
        [else  (f ω z γ)]))

(: run-rand-preimage-fun (Rand-Preimage-Fun Domain-Set Domain-Set -> Domain-Set))
(define (run-rand-preimage-fun f Γ K)
  (cond [(or (empty-set? Γ) (empty-set? K))  empty-set]
        [else  (f Γ K)]))

(: run-rand-preimage (Rand-Preimage Domain-Set Domain-Set -> Domain-Set))
(define (run-rand-preimage f-pre Γ K)
  (cond [(or (empty-preimage? f-pre) (empty-set? Γ) (empty-set? K))  empty-set]
        [else
         (match-define (rand-preimage Γf Kf f) f-pre)
         (cond [(and check-rand-preimage-arguments? (not (domain-set-subseteq? Γ Γf)))
                (raise-argument-error 'run-rand-preimage (format "domain subset of ~e" Γf)
                                      1 f-pre Γ K)]
               [(and check-rand-preimage-arguments? (not (domain-set-subseteq? K Kf)))
                (raise-argument-error 'run-rand-preimage (format "range subset of ~e" Kf)
                                      2 f-pre Γ K)]
               [else
                (f Γ K)])]))

(: run-rand-computation (Rand-Computation Domain-Set -> Rand-Preimage))
(define (run-rand-computation comp Γ)
  (cond [(or (empty-set? Γ))  empty-preimage]
        [else  (comp Γ)]))

(: make-rand-preimage (Domain-Set Domain-Set Rand-Preimage-Fun -> Rand-Preimage))
(define (make-rand-preimage Γ K f)
  (cond [(or (empty-set? Γ) (empty-set? K))  empty-preimage]
        [else  (nonempty-rand-preimage Γ K f)]))

(: empty-rand-preimage-fun Rand-Preimage-Fun)
(define (empty-rand-preimage-fun Γ K) empty-set)

(define-match-expander rand-preimage
  (λ (stx)
    (syntax-case stx ()
      [(_ Γ K f)
       (syntax/loc stx
         (or (nonempty-rand-preimage Γ K f)
             (and (? empty-preimage?)
                  (app (λ (_) empty-set) Γ)
                  (app (λ (_) empty-set) K)
                  (app (λ (_) empty-rand-preimage-fun) f))
             (and (app (λ: ([_ : Rand-Preimage]) empty-set) Γ)
                  (app (λ (_) empty-set) K)
                  (app (λ (_) empty-rand-preimage-fun) f))))]))
  (make-head-form #'make-rand-preimage))

;; ===================================================================================================
;; Caching wrappers
#|
(: rand-preimage (Omega-Rect Nonempty-Set Branches-Rect Nonempty-Set Rand-Preimage-Fun
                             -> Rand-Preimage-Fun))
;; Wraps a Rand-Preimage-Fun with code that ensures the argument is a subset of the range; also
;; caches return values
(define ((rand-preimage Ω Γ Z K pre) Zsub Ksub)
  (let ([Zsub  (branches-rect-intersect Z Zsub)]
        [Ksub  (set-intersect K Ksub)])
    (cond [(or (empty-set? Zsub) (empty-set? Ksub))
           (values empty-set empty-set empty-set)]
          [(not rand-cache-preimages?)
           (pre Zsub Ksub)]
          [(and (eq? Zsub Z) (eq? Ksub K))
           (when rand-preimage-stats? (increment-cache-stat 'rand-preimage-hits))
           (values Ω Z Γ)]
          [else
           (when rand-preimage-stats?
             (increment-cache-stat 'rand-preimage-misses)
             (when (and (not (eq? Zsub Z)) (equal? Zsub Z))
               (increment-cache-stat 'rand-preimage-misses/bad-Z))
             (when (and (not (eq? Ksub K)) (equal? Ksub K))
               (increment-cache-stat 'rand-preimage-misses/bad-K)))
           (pre Zsub Ksub)])))

(: cached-rand-computation (Rand-Computation -> Rand-Computation))
(define (cached-rand-computation comp)
  (define: last-Ω : (U #f Omega-Rect)  #f)
  (define: last-Z : (U #f Branches-Rect) #f)
  (define: last-Γ : (U #f Nonempty-Set)  #f)
  (define: last-m : (U #f Rand-Computation-Meaning)  #f)
  (λ (Ω Z Γ)
    (define cached-m last-m)
    (cond
      [(empty-set? Ω)  empty-meaning]
      [(empty-set? Z)  empty-meaning]
      [(empty-set? Γ)  empty-meaning]
      [(not rand-cache-computations?)  (comp Ω Z Γ)]
      [(and (eq? Ω last-Ω) (eq? Z last-Z) (eq? Γ last-Γ) cached-m)
       (when rand-computation-stats? (increment-cache-stat 'rand-computation-hits))
       cached-m]
      [else
       (when rand-computation-stats?
         (increment-cache-stat 'rand-computation-misses)
         (when (and (not (eq? Ω last-Ω)) (equal? Ω last-Ω))
           (increment-cache-stat 'rand-computation-misses/bad-Ω))
         (when (and (not (eq? Z last-Z)) (equal? Z last-Z))
           (increment-cache-stat 'rand-computation-misses/bad-Z))
         (when (and (not (eq? Γ last-Γ)) (equal? Γ last-Γ))
           (increment-cache-stat 'rand-computation-misses/bad-Γ)))
       (set! last-Ω Ω)
       (set! last-Z Z)
       (set! last-Γ Γ)
       (let ([cached-m  (comp Ω Z Γ)])
         (set! last-m cached-m)
         cached-m)])))
|#
;; ===================================================================================================
;; Arrow composition (reverse composition)

(: rand-rcompose/fwd (Rand-Forward-Fun Rand-Forward-Fun -> Rand-Forward-Fun))
(define ((rand-rcompose/fwd f-fwd g-fwd) ω z γ)
  (run-rand-forward-fun g-fwd ω z (run-rand-forward-fun f-fwd ω z γ)))

(: rand-rcompose/pre (Rand-Preimage Rand-Preimage Domain-Set -> Rand-Preimage-Fun))
(define ((rand-rcompose/pre f-pre g-pre Γg) Γ K)
  (run-rand-preimage f-pre Γ (run-rand-preimage g-pre Γg K)))

(: rand-rcompose/comp (Rand-Computation Rand-Computation -> Rand-Computation))
(define ((rand-rcompose/comp f-comp g-comp) Γ)
  (match-define (and f-pre (rand-preimage Γf Kf f)) (f-comp Γ))
  (match-define (and g-pre (rand-preimage Γg Kg g)) (run-rand-computation g-comp Kf))
  (rand-preimage Γf Kg (rand-rcompose/pre f-pre g-pre Γg)))

(: rand-rcompose/arr (rand-expression rand-expression -> rand-expression))
(define (rand-rcompose/arr f-expr g-expr)
  (rand-expression
   (λ (r)
     (match-define (rand-expression-meaning f-idxs f-fwd f-comp)
       (run-rand-expression f-expr (cons 0 r)))
     (match-define (rand-expression-meaning g-idxs g-fwd g-comp)
       (run-rand-expression g-expr (cons 1 r)))
     (rand-expression-meaning (append f-idxs g-idxs)
                              (rand-rcompose/fwd f-fwd g-fwd)
                              (rand-rcompose/comp f-comp g-comp)))))

;; ===================================================================================================
;; Pairing

(: rand-pair/fwd (Rand-Forward-Fun Rand-Forward-Fun -> Rand-Forward-Fun))
(define ((rand-pair/fwd fst-fwd snd-fwd) ω z γ)
  (define k1 (fst-fwd ω z γ))
  (cond [(bottom? k1)  k1]
        [else  (define k2 (snd-fwd ω z γ))
               (cond [(bottom? k2)  k2]
                     [else  (cons k1 k2)])]))

(: rand-pair/pre (Rand-Preimage Rand-Preimage -> Rand-Preimage-Fun))
(define ((rand-pair/pre pre1 pre2) Γ K)
  (define K1 (domain-set-pair-ref K 'fst))
  (define K2 (domain-set-pair-ref K 'snd))
  (run-rand-preimage pre1 (run-rand-preimage pre2 Γ K2) K1))
#;
(define ((rand-pair/pre pre1 pre2) Γ K)
  (match-define (nonempty-domain-set _1 _2 A) Γ)
  (match-define (nonempty-domain-set Ω Z B) K)
  (let ([Γ   (domain-set Ω Z A)]
        [K2  (domain-set Ω Z (set-pair-ref B 'snd))])
    (match-let ([(and Γ (domain-set Ω Z A))  (run-rand-preimage pre2 Γ K2)])
      (define K1 (domain-set Ω Z (set-pair-ref B 'fst)))
      (run-rand-preimage pre1 Γ K1))))

(: rand-pair/comp (Rand-Computation Rand-Computation -> Rand-Computation))
(define ((rand-pair/comp comp1 comp2) Γ)
  (match-define (and pre1 (rand-preimage Γ1 K1 f1)) (comp1 Γ))
  (match-define (and pre2 (rand-preimage Γ2 K2 f2)) (run-rand-computation comp2 Γ1))
  (rand-preimage Γ2 (domain-set-pair K1 K2) (rand-pair/pre pre1 pre2)))

(: rand-pair/arr (rand-expression rand-expression -> rand-expression))
(define (rand-pair/arr expr1 expr2)
  (rand-expression
   (λ (r)
     (match-define (rand-expression-meaning idxs1 fwd1 comp1) (run-rand-expression expr1 (cons 0 r)))
     (match-define (rand-expression-meaning idxs2 fwd2 comp2) (run-rand-expression expr2 (cons 1 r)))
     (rand-expression-meaning (append idxs1 idxs2)
                              (rand-pair/fwd fwd1 fwd2)
                              (rand-pair/comp comp1 comp2)))))

;; ===================================================================================================
;; Conditionals

(: rand-c/fwd (Value -> Rand-Forward-Fun))
(define ((rand-c/fwd x) ω z γ) (if (bottom? γ) γ x))

(: rand-c/pre Rand-Preimage-Fun)
(define (rand-c/pre Γ K) (if (empty-set? K) K Γ))

(: rand-c/comp (Nonempty-Set -> Rand-Computation))
(define ((rand-c/comp X) Γ)
  (match-define (nonempty-domain-set Ω Z Ain) Γ)
  (define K (nonempty-domain-set Ω Z X))
  (rand-preimage Γ K rand-c/pre))

(: rand-c/arr (Value -> rand-expression))
(define (rand-c/arr x)
  (define fwd (rand-c/fwd x))
  (define X (value->singleton x))
  (rand-expression (λ (r) (rand-expression-meaning empty fwd (rand-c/comp X)))))


(: rand-switch/fwd (Omega-Index (-> Rand-Forward-Fun) (-> Rand-Forward-Fun) -> Rand-Forward-Fun))
(define ((rand-switch/fwd idx t-fwd f-fwd) ω z γ)
  (match-let ([(cons b γ)  γ])
    (define zb (branches-ref z idx))
    (cond [(not (boolean? b))  (bottom (delay (format "if: expected Boolean condition; given ~e" b)))]
          [(and (eq? b #t) (or (eq? zb #t) (eq? zb 'either)))  ((t-fwd) ω z (cons #t γ))]
          [(and (eq? b #f) (or (eq? zb #f) (eq? zb 'either)))  ((f-fwd) ω z (cons #f γ))]
          [else  (bottom (delay (format "if: missed branch at index ~a" idx)))])))

(: rand-switch/comp (Omega-Index (-> Rand-Computation) (-> Rand-Computation) -> Rand-Computation))
(define ((rand-switch/comp idx t-comp f-comp) Γ)
  (match-define (nonempty-domain-set Ω Z A) Γ)
  (match (set-intersect A (set-pair (branches-rect-ref Z idx) universe))
    [(and A (pair-rect Ab Atf))
     (define t? (set-member? Ab #t))
     (define f? (set-member? Ab #f))
     (cond
       [(and t? f?)
        (define Γ (nonempty-domain-set Ω Z A))
        (define K (nonempty-domain-set Ω Z universe))
        (rand-preimage Γ K (λ (Γ K) Γ))]
       [t?  ((t-comp) (nonempty-domain-set Ω (branches-rect-set Z idx trues) (set-pair trues Atf)))]
       [f?  ((f-comp) (nonempty-domain-set Ω (branches-rect-set Z idx falses) (set-pair falses Atf)))]
       [else  empty-preimage])]
    [_
     empty-preimage]))

(: rand-switch/arr (rand-expression rand-expression -> rand-expression))
(define (rand-switch/arr t-expr f-expr)
  (rand-expression
   (λ (r)
     (define idx (reverse r))
     (define t-meaning (delay (run-rand-expression t-expr (cons 0 r))))
     (define f-meaning (delay (run-rand-expression f-expr (cons 1 r))))
     (rand-expression-meaning
      (list (if-indexes idx
                        (λ () (rand-expression-meaning-indexes (force t-meaning)))
                        (λ () (rand-expression-meaning-indexes (force f-meaning)))))
      (rand-switch/fwd idx
                       (λ () (rand-expression-meaning-forward (force t-meaning)))
                       (λ () (rand-expression-meaning-forward (force f-meaning))))
      (rand-switch/comp idx
                        (λ () (rand-expression-meaning-computation (force t-meaning)))
                        (λ () (rand-expression-meaning-computation (force f-meaning))))))))

;; ===================================================================================================
;; Random

(: random/fwd (Omega-Index -> Rand-Forward-Fun))
(define ((random/fwd idx) ω z γ)
  (omega-ref ω idx))

(: random/pre (Omega-Index -> Rand-Preimage-Fun))
(define ((random/pre idx) Γ K)
  (match-define (nonempty-domain-set Ωin  Zin  Ain)  Γ)
  (match-define (nonempty-domain-set Ωout Zout Aout) K)
  (let ([Aout  (real-set-intersect unit-interval (set-take-reals Aout))])
    (cond [(empty-real-set? Aout)  empty-set]
          [else  (domain-set (omega-rect-intersect Ωin (omega-rect-set Ωout idx Aout))
                             (branches-rect-intersect Zin Zout)
                             Ain)])))

(: random/comp (Omega-Index -> Rand-Computation))
(define ((random/comp idx) Γ)
  (match-define (nonempty-domain-set Ω Z A) Γ)
  (define K (nonempty-domain-set Ω Z (omega-rect-ref Ω idx)))
  (rand-preimage Γ K (random/pre idx)))

(: random/arr rand-expression)
(define random/arr
  (rand-expression
   (λ (r)
     (define idx (reverse r))
     (rand-expression-meaning (list (interval-index idx #f))
                              (random/fwd idx)
                              (random/comp idx)))))
