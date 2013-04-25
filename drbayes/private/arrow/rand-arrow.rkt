#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "../untyped-utils.rkt"
         "indexes.rkt"
         "arrow-common.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Random expressions

(define-type Rand-Forward-Fun (Omega Branches Value -> Maybe-Value))
(define-type Rand-Preimage-Fun
  (Branches-Rect Nonempty-Set -> (Values Maybe-Omega-Rect Maybe-Branches-Rect Set)))

;; A computation is a function from a domain and branches to its meaning
(define-type Rand-Computation-Meaning (U Empty-Meaning rand-computation-meaning))
(define-type Rand-Computation (Omega-Rect Branches-Rect Nonempty-Set -> Rand-Computation-Meaning))

;; A computation means:
;;  1. A branches rectangle (which bounds the branches the forward computation can take)
;;  2. The approximate range of its forward function
;;  3. A function that computes approximate preimages under its forward function
(struct: rand-computation-meaning ([Z : Branches-Rect]
                                   [K : Nonempty-Set]
                                   [preimage : Rand-Preimage-Fun])
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

(: run-rand-forward (Rand-Forward-Fun Omega Branches Maybe-Value -> Maybe-Value))
(define (run-rand-forward f ω z γ)
  (cond [(bottom? γ)  γ]
        [else  (f ω z γ)]))

(: run-rand-preimage (Rand-Preimage-Fun Maybe-Branches-Rect Set
                                        -> (Values Maybe-Omega-Rect Maybe-Branches-Rect Set)))
(define (run-rand-preimage pre Z K)
  (if (or (empty-set? Z) (empty-set? K))
      (values empty-set empty-set empty-set)
      (pre Z K)))

;; ===================================================================================================
;; Caching wrappers

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

;; ===================================================================================================
;; Arrow composition (reverse composition)

(: rand-rcompose/fwd (Rand-Forward-Fun Rand-Forward-Fun -> Rand-Forward-Fun))
(define ((rand-rcompose/fwd f-fwd g-fwd) ω z γ)
  (let* ([kf  (f-fwd ω z γ)]
         [kg  (run-rand-forward g-fwd ω z kf)])
    kg))

(: rand-rcompose/pre (Rand-Preimage-Fun Rand-Preimage-Fun Omega-Rect -> Rand-Preimage-Fun))
(define ((rand-rcompose/pre f-pre g-pre Ω) Z Kg)
  (define Zf (branches-rect-fst Z))
  (define Zg (branches-rect-snd Z))
  (let*-values ([(Ωg Zg Kf)  (g-pre Zg Kg)]
                [(Ωf Zf Γf)  (run-rand-preimage f-pre Zf Kf)]
                [(Ω)  (unit-omega-rect-node/last Ω Ωf Ωg)]
                [(Z)  (branches-rect-node/last Z booleans Zf Zg)])
    (values Ω Z Γf)))

(: rand-rcompose/comp (Rand-Computation Rand-Computation -> Rand-Computation))
(define (rand-rcompose/comp f-comp g-comp)
  (cached-rand-computation
   (λ (Ω Z Γf)
     (define Ωf (omega-rect-fst Ω))
     (define Zf (branches-rect-fst Z))
     (define f-meaning (f-comp Ωf Zf Γf))
     (cond
       [(empty-meaning? f-meaning)  empty-meaning]
       [else
        (match-define (rand-computation-meaning Zf Γg f-pre) f-meaning)
        (define Ωg (omega-rect-snd Ω))
        (define Zg (branches-rect-snd Z))
        (define g-meaning (g-comp Ωg Zg Γg))
        (cond
          [(empty-meaning? g-meaning)  empty-meaning]
          [else
           (match-define (rand-computation-meaning Zg Kg g-pre) g-meaning)
           (cond [(or (empty-set? Zf) (empty-set? Zg))  empty-meaning]
                 [else
                  (let ([Z  (branches-rect-node/last Z booleans Zf Zg)])
                    (define pre (rand-preimage Ω Γf Z Kg (rand-rcompose/pre f-pre g-pre Ω)))
                    (rand-computation-meaning Z Kg pre))])])]))))

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

(: rand-pair/pre (Rand-Preimage-Fun Rand-Preimage-Fun Omega-Rect Omega-Rect Omega-Rect
                                    -> Rand-Preimage-Fun))
(define ((rand-pair/pre pre1 pre2 Ω Ω1 Ω2) Z K)
  (define K1 (set-pair-ref K 'fst))
  (define K2 (set-pair-ref K 'snd))
  (define Z1 (branches-rect-fst Z))
  (define Z2 (branches-rect-snd Z))
  (let-values ([(Ω1 Z1 Γ1)  (run-rand-preimage pre1 Z1 K1)]
               [(Ω2 Z2 Γ2)  (run-rand-preimage pre2 Z2 K2)])
    (values (unit-omega-rect-node/last Ω Ω1 Ω2)
            (branches-rect-node/last Z booleans Z1 Z2)
            (set-intersect Γ1 Γ2))))

(: rand-pair/comp (Rand-Computation Rand-Computation -> Rand-Computation))
(define (rand-pair/comp comp1 comp2)
  (cached-rand-computation
   (λ (Ω Z Γ)
     (define Ω1 (omega-rect-fst Ω))
     (define Ω2 (omega-rect-snd Ω))
     (define Z1 (branches-rect-fst Z))
     (define Z2 (branches-rect-snd Z))
     (define meaning1 (comp1 Ω1 Z1 Γ))
     (define meaning2 (if (empty-meaning? meaning1) empty-meaning (comp2 Ω2 Z2 Γ)))
     (cond
       [(empty-meaning? meaning2)  empty-meaning]
       [else
        (match-define (rand-computation-meaning Z1 K1 pre1) meaning1)
        (match-define (rand-computation-meaning Z2 K2 pre2) meaning2)
        (let ([Z  (branches-rect-node/last Z booleans Z1 Z2)]
              [K  (set-pair K1 K2)])
          (define pre (rand-preimage Ω Γ Z K (rand-pair/pre pre1 pre2 Ω Ω1 Ω2)))
          (rand-computation-meaning Z K pre))]))))

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

(: rand-switch/fwd (Omega-Index (-> Rand-Forward-Fun) (-> Rand-Forward-Fun) -> Rand-Forward-Fun))
(define ((rand-switch/fwd idx t-fwd f-fwd) ω z γ)
  (match-let ([(cons b γ)  γ])
    (define zb (branches-ref z idx))
    (cond [(not (boolean? b))  (bottom (delay (format "if: expected Boolean condition; given ~e" b)))]
          [(and (eq? b #t) (or (eq? zb #t) (eq? zb 'either)))  ((t-fwd) ω z γ)]
          [(and (eq? b #f) (or (eq? zb #f) (eq? zb 'either)))  ((f-fwd) ω z γ)]
          [else  (bottom (delay (format "if: missed branch at index ~a" idx)))])))

(: rand-switch-t/pre ((-> Rand-Computation) Omega-Rect Omega-Rect Omega-Rect Nonempty-Set
                                            -> Rand-Preimage-Fun))
(define ((rand-switch-t/pre comp Ω Ωt Ωf Γ) Z K)
  (define Zt (branches-rect-fst Z))
  (define meaning ((comp) Ωt Zt Γ))
  (cond
    [(empty-meaning? meaning)  (values empty-set empty-set empty-set)]
    [else
     (match-define (rand-computation-meaning Zt Kt pre) meaning)
     (define Zf (branches-rect-snd Z))
     (let*-values ([(Ωt Zt Γ)  (run-rand-preimage pre Zt (set-intersect K Kt))]
                   [(Ω)  (unit-omega-rect-node/last Ω Ωt Ωf)]
                   [(Z)  (branches-rect-node/last Z trues Zt Zf)]
                   [(Γ)  (set-pair trues Γ)])
       (values Ω Z Γ))]))

(: rand-switch-f/pre ((-> Rand-Computation) Omega-Rect Omega-Rect Omega-Rect Nonempty-Set
                                            -> Rand-Preimage-Fun))
(define ((rand-switch-f/pre comp Ω Ωt Ωf Γ) Z K)
  (define Zf (branches-rect-snd Z))
  (define meaning ((comp) Ωf Zf Γ))
  (cond
    [(empty-meaning? meaning)  (values empty-set empty-set empty-set)]
    [else
     (match-define (rand-computation-meaning Zf Kf pre) meaning)
     (define Zt (branches-rect-fst Z))
     (let*-values ([(Ωf Zf Γ)  (run-rand-preimage pre Zf (set-intersect K Kf))]
                   [(Ω)  (unit-omega-rect-node/last Ω Ωt Ωf)]
                   [(Z)  (branches-rect-node/last Z falses Zt Zf)]
                   [(Γ)  (set-pair falses Γ)])
       (values Ω Z Γ))]))

(: rand-switch-tf/pre ((-> Rand-Computation)
                       (-> Rand-Computation)
                       Omega-Rect Omega-Rect Omega-Rect
                       Nonempty-Set Nonempty-Set
                       -> Rand-Preimage-Fun))
(define ((rand-switch-tf/pre t-comp f-comp Ω Ωt Ωf Γ Γtf) Z K)
  (define b (branches-rect-value Z))
  (cond [(eq? b booleans)  (values Ω Z Γ)]
        [(eq? b trues)     ((rand-switch-t/pre t-comp Ω Ωt Ωf Γtf) Z K)]
        [(eq? b falses)    ((rand-switch-f/pre f-comp Ω Ωt Ωf Γtf) Z K)]
        [else  (values empty-set empty-set empty-set)]))

(: rand-switch/comp ((-> Rand-Computation) (-> Rand-Computation) -> Rand-Computation))
(define (rand-switch/comp t-comp f-comp)
  (cached-rand-computation
   (λ (Ω Z orig-Γ)
     (define Γ (set-intersect orig-Γ (pair-rect (branches-rect-value Z) universe)))
     (match Γ
       [(and Γ (pair-rect Γb Γtf))
        (define Ωt (omega-rect-fst Ω))
        (define Ωf (omega-rect-snd Ω))
        (define Zt (branches-rect-fst Z))
        (define Zf (branches-rect-snd Z))
        
        (cond
          [(eq? Γb booleans)
           (define K universe)
           (define pre (rand-preimage Ω Γ Z K (rand-switch-tf/pre t-comp f-comp Ω Ωt Ωf Γ Γtf)))
           (rand-computation-meaning Z K pre)]
          [(eq? Γb trues)
           (define t-meaning ((t-comp) Ωt Zt Γtf))
           (cond [(empty-meaning? t-meaning)  empty-meaning]
                 [else
                  (match-define (rand-computation-meaning Zt Kt t-pre) t-meaning)
                  (let ([Z  (branches-rect-node trues Zt Zf)])
                    (define pre (rand-preimage Ω Γ Z Kt (rand-switch-t/pre t-comp Ω Ωt Ωf Γtf)))
                    (rand-computation-meaning Z Kt pre))])]
          [(eq? Γb falses)
           (define f-meaning ((f-comp) Ωf Zf Γtf))
           (cond [(empty-meaning? f-meaning)  empty-meaning]
                 [else
                  (match-define (rand-computation-meaning Zf Kf f-pre) f-meaning)
                  (let ([Z  (branches-rect-node falses Zt Zf)])
                    (define pre (rand-preimage Ω Γ Z Kf (rand-switch-f/pre f-comp Ω Ωt Ωf Γtf)))
                    (rand-computation-meaning Z Kf pre))])]
          [else
           ;; Shouldn't be possible
           (raise-argument-error 'switch/comp "Boolean-Rect" Γb)])]
       [_
        empty-meaning]))))

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
      (rand-switch/comp (λ () (rand-expression-meaning-computation (force t-meaning)))
                        (λ () (rand-expression-meaning-computation (force f-meaning))))))))

;; ===================================================================================================
;; Random

(: random/fwd (Omega-Index -> Rand-Forward-Fun))
(define ((random/fwd idx) ω z γ)
  (omega-ref ω idx))

(: random/pre (Omega-Rect Nonempty-Set -> Rand-Preimage-Fun))
(define (random/pre Ω Γ)
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (Z K)
    (cond [(interval*? K)  (values (omega-rect-node K Ω1 Ω2) Z Γ)]
          [else            (values empty-set empty-set empty-set)])))

(: random/comp (-> Rand-Computation))
(define (random/comp)
  (cached-rand-computation
   (λ (Ω Z Γ)
     (define K (omega-rect-value Ω))
     (rand-computation-meaning Z K (rand-preimage Ω Γ Z K (random/pre Ω Γ))))))

(: random/arr rand-expression)
(define random/arr
  (rand-expression
   (λ (r)
     (define idx (reverse r))
     (rand-expression-meaning (list (interval-index idx #f))
                              (random/fwd idx)
                              (random/comp)))))

#;
((rand-computation-meaning-preimage
  (assert
   ((rand-expression-meaning-computation
     (run-rand-expression (rand-rcompose/arr (random/ 0.6)
                                             (rand-switch/arr (->rand (c/arr #t))
                                                              (->rand (c/arr #f))))))
    omega-rect (branches-rect-set branches-rect '(1) trues) null-set)
   rand-computation-meaning?))
 (branches-rect-set branches-rect '(1) trues) universe)
