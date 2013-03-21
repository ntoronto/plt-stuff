#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "indexes.rkt")

(provide (all-defined-out))

(define cache-preimages? #t)
(define prim-cache-preimages? #t)
(define pair-cache-preimages? #t)
(define cache-computations? #t)
(define prim-cache-computations? #t)

(define preimage-stats? #f)
(define prim-preimage-stats? #f)
(define pair-preimage-stats? #f)
(define computation-stats? #f)
(define prim-computation-stats? #f)

(define: cache-stats : (HashTable Symbol Natural)  (make-hasheq empty))

(: increment-cache-stat (Symbol -> Void))
(define (increment-cache-stat name)
  (hash-set! cache-stats name (+ 1 (hash-ref cache-stats name (λ () 0)))))

(: get-cache-stats (-> (Listof (Pair Symbol Natural))))
(define (get-cache-stats)
  ((inst sort (Pair Symbol Natural) String)
   (hash-map cache-stats (λ: ([k : Symbol] [v : Natural]) (cons k v)))
   string<?
   #:key (λ: ([kv : (Pair Symbol Natural)]) (symbol->string (car kv)))
   #:cache-keys? #t))

(struct: forward-fail ([reason : String]) #:transparent)

;; ===================================================================================================
;; Expression type

;; An expression is a function from a reversed index to the expression's meaning
(struct: expression ([fun : (Omega-Index -> expression-meaning)])
  #:transparent)

;; Expressions are currently wrapped in a struct so they can be recognized by its predicate (see
;; "language.rkt"), but this may not be necessary in the future

;; An expression means:
;;  1. Lazy indexes of random variables and branch points
;;  2. A forward function
;;  3. A computation, which is used to compute preimages
(struct: expression-meaning ([indexes : Indexes]
                             [forward : Forward-Fun]
                             [computation : Computation])
  #:transparent)

(define-type Forward-Fun (Omega Branches Value -> Value))

(: run-expression (case-> (expression -> expression-meaning)
                          (expression Omega-Index -> expression-meaning)))
(define (run-expression e [r empty])
  ((expression-fun e) r))

;; A computation is a function from a domain and branches to its meaning
(define-type Computation (Maybe-Omega-Rect Maybe-Branches-Rect Set -> computation-meaning))

;; A computation means:
;;  1. A branches rectangle (which bounds the branches the forward computation can take)
;;  2. The approximate range of its forward function
;;  3. A function that computes approximate preimages under its forward function
(struct: computation-meaning ([Z : Maybe-Branches-Rect]
                              [K : Set]
                              [preimage : Preimage-Fun])
  #:transparent)

(define-type Preimage-Fun
  (Maybe-Branches-Rect Set -> (Values Maybe-Omega-Rect Maybe-Branches-Rect Set)))

;; ===================================================================================================
;; Convenience and caching wrappers

(define-type Simple-Computation (Omega-Rect Branches-Rect Nonempty-Set -> computation-meaning))
(define-type Simple-Preimage-Fun
  (Branches-Rect Nonempty-Set -> (Values Maybe-Omega-Rect Maybe-Branches-Rect Set)))

(define-type Prim-Computation (Nonempty-Set -> computation-meaning))
(define-type Prim-Preimage-Fun (Nonempty-Set -> Set))

(: run-prim-preimage (Preimage-Fun Set -> Set))
(define (run-prim-preimage pre K)
  (let-values ([(_Ω _Z Γ)  (pre branches-rect K)])
    Γ))

(: run-prim-computation (Computation Set -> (Values Set Preimage-Fun)))
(define (run-prim-computation comp Γ)
  (match-define (computation-meaning _Z K pre) (comp omega-rect branches-rect Γ))
  (values K pre))

(: simple-preimage (Maybe-Omega-Rect Set Maybe-Branches-Rect Set Simple-Preimage-Fun
                                     -> Preimage-Fun))
;; Wraps a Prim-Preimage-Fun with code that ensures the argument is a subset of the range and is
;; nonempty
(define ((simple-preimage Ω Γ Z K pre) Zsub Ksub)
  (let ([Zsub  (branches-rect-intersect Z Zsub)]
        [Ksub  (set-intersect K Ksub)])
    (cond [(or (empty-set? Zsub) (empty-set? Ksub))
           (values empty-set empty-set empty-set)]
          [(not cache-preimages?)
           (pre Zsub Ksub)]
          [(and (eq? Zsub Z) (eq? Ksub K))
           (when preimage-stats? (increment-cache-stat 'preimage-hits))
           (values Ω Z Γ)]
          [else
           (when preimage-stats?
             (increment-cache-stat 'preimage-misses)
             (when (and (not (eq? Zsub Z)) (equal? Zsub Z))
               (increment-cache-stat 'preimage-misses/bad-Z))
             (when (and (not (eq? Ksub K)) (equal? Ksub K))
               (increment-cache-stat 'preimage-misses/bad-K)))
           (pre Zsub Ksub)])))

(: prim-preimage (Set Set Prim-Preimage-Fun -> Preimage-Fun))
;; Like `simple-preimage' but for preimages under primitives
(define ((prim-preimage Γ K pre) Zsub Ksub)
  (let ([Ksub  (set-intersect K Ksub)])
    (cond [(empty-set? Ksub)
           (values empty-set empty-set empty-set)]
          [(not prim-cache-preimages?)
           (values omega-rect branches-rect (pre Ksub))]
          [(eq? Ksub K)
           (when prim-preimage-stats? (increment-cache-stat 'prim-preimage-hits))
           (values omega-rect branches-rect Γ)]
          [else
           (when prim-preimage-stats?
             (increment-cache-stat 'prim-preimage-misses)
             (when (and (not (eq? Ksub K)) (equal? Ksub K))
               (increment-cache-stat 'prim-preimage-misses/bad-K)))
           (values omega-rect branches-rect (pre Ksub))])))

(: cached-computation (Simple-Computation -> Computation))
(define (cached-computation comp)
  (define: last-Ω : (U #f Omega-Rect)  #f)
  (define: last-Z : (U #f Branches-Rect) #f)
  (define: last-Γ : (U #f Nonempty-Set)  #f)
  (define: last-m : (U #f computation-meaning)  #f)
  (λ (Ω Z Γ)
    (define cached-m last-m)
    (cond
      [(or (empty-set? Ω) (empty-set? Z) (empty-set? Γ))  (bottom/comp Ω Z Γ)]
      [(not cache-computations?)  (comp Ω Γ)]
      [(and (eq? Ω last-Ω) (eq? Z last-Z) (eq? Γ last-Γ) cached-m)
       (when computation-stats? (increment-cache-stat 'computation-hits))
       cached-m]
      [else
       (when computation-stats?
         (increment-cache-stat 'computation-misses)
         (when (and (not (eq? Ω last-Ω)) (equal? Ω last-Ω))
           (increment-cache-stat 'computation-misses/bad-Ω))
         (when (and (not (eq? Z last-Z)) (equal? Z last-Z))
           (increment-cache-stat 'computation-misses/bad-Z))
         (when (and (not (eq? Γ last-Γ)) (equal? Γ last-Γ))
           (increment-cache-stat 'computation-misses/bad-Γ)))
       (set! last-Ω Ω)
       (set! last-Z Z)
       (set! last-Γ Γ)
       (let ([cached-m  (comp Ω Z Γ)])
         (set! last-m cached-m)
         cached-m)])))

(: cached-prim-computation (Set Prim-Computation -> Computation))
(define (cached-prim-computation domain comp)
  (define: last-Γ : (U #f Nonempty-Set)  #f)
  (define: last-m : (U #f computation-meaning)  #f)
  (λ (Ω Z Γ)
    (let ([Γ  (set-intersect Γ domain)])
      (define cached-m last-m)
      (cond
        [(or (empty-set? Ω) (empty-set? Z) (empty-set? Γ))  (bottom/comp Ω Z Γ)]
        [(not prim-cache-computations?)  (comp Γ)]
        [(and (eq? Γ last-Γ) cached-m)
         (when prim-computation-stats? (increment-cache-stat 'prim-computation-hits))
         cached-m]
        [else
         (when prim-computation-stats?
           (increment-cache-stat 'prim-computation-misses)
           (when (and (not (eq? Γ last-Γ)) (equal? Γ last-Γ))
             (increment-cache-stat 'prim-computation-misses/bad-Γ)))
         (set! last-Γ Γ)
         (let ([cached-m  (comp Γ)])
           (set! last-m cached-m)
           cached-m)]))))

;; ===================================================================================================
;; Basic primitives

;; Bottom function: has empty range

(: bottom/fwd Forward-Fun)
(define (bottom/fwd ω z γ)
  (raise (forward-fail "bottom")))

(: bottom/pre Preimage-Fun)
(define (bottom/pre Z K)
  (values empty-set empty-set empty-set))

(: bottom/comp Computation)
(define (bottom/comp _Ω _Z _Γ)
  (computation-meaning empty-set empty-set bottom/pre))

(define bottom/arr
  (expression (λ (r) (expression-meaning empty bottom/fwd bottom/comp))))

;; Identity function

(: id/fwd Forward-Fun)
(define (id/fwd ω z γ) γ)

(: id/pre Prim-Preimage-Fun)
(define (id/pre K) K)

(: id/comp (-> Computation))
(define (id/comp)
  (cached-prim-computation
   universe
   (λ (Γ) (computation-meaning branches-rect Γ (prim-preimage Γ Γ id/pre)))))

(define id/arr
  (expression (λ (r) (expression-meaning empty id/fwd (id/comp)))))

;; Constant functions

(: c/fwd (Value -> Forward-Fun))
(define ((c/fwd x) ω z γ) x)

(: c/pre (Nonempty-Set -> Prim-Preimage-Fun))
(define ((c/pre X) K)
  (if (empty-set? K) K X))

(: c/comp (Nonempty-Set Nonempty-Set -> Computation))
(define (c/comp domain X)
  (cached-prim-computation
   domain
   (λ (Γ) (computation-meaning branches-rect X (prim-preimage Γ X (c/pre X))))))

(: c/arr (case-> (Value -> expression)
                 (Value Nonempty-Set -> expression)))
(define (c/arr x [domain universe])
  (define fwd (c/fwd x))
  (define X (value->singleton x))
  (expression (λ (r) (expression-meaning empty fwd (c/comp domain X)))))

;; ===================================================================================================
;; Arrow composition (reverse composition)

(: rcompose/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((rcompose/fwd f-fwd g-fwd) ω z γ)
  (let* ([kf  (f-fwd ω z γ)]
         [kg  (g-fwd ω z kf)])
    kg))

(: rcompose/pre (Preimage-Fun Preimage-Fun Omega-Rect -> Simple-Preimage-Fun))
(define ((rcompose/pre f-pre g-pre Ω) Z Kg)
  (define Zf (branches-rect-fst Z))
  (define Zg (branches-rect-snd Z))
  (let*-values ([(Ωg Zg Kf)  (g-pre Zg Kg)]
                [(Ωf Zf Γf)  (f-pre Zf Kf)]
                [(Ω)  (unit-omega-rect-node/last Ω Ωf Ωg)]
                [(Z)  (branches-rect-node/last Z booleans Zf Zg)])
    (values Ω Z Γf)))

(: rcompose/comp (Computation Computation -> Computation))
(define (rcompose/comp f-comp g-comp)
  (cached-computation
   (λ (Ω Z Γf)
     (define Ωf (omega-rect-fst Ω))
     (define Ωg (omega-rect-snd Ω))
     (define Zf (branches-rect-fst Z))
     (define Zg (branches-rect-snd Z))
     (match-let* ([(computation-meaning Zf Γg f-pre)  (f-comp Ωf Zf Γf)]
                  [(computation-meaning Zg Kg g-pre)  (g-comp Ωg Zg Γg)]
                  [Z  (branches-rect-node/last Z booleans Zf Zg)])
       (define pre (simple-preimage Ω Γf Z Kg (rcompose/pre f-pre g-pre Ω)))
       (computation-meaning Z Kg pre)))))

(: rcompose/arr (expression expression -> expression))
(define (rcompose/arr f-expr g-expr)
  (expression
   (λ (r)
     (match-define (expression-meaning f-idxs f-fwd f-comp) (run-expression f-expr (cons 0 r)))
     (match-define (expression-meaning g-idxs g-fwd g-comp) (run-expression g-expr (cons 1 r)))
     (expression-meaning (append f-idxs g-idxs)
                         (rcompose/fwd f-fwd g-fwd)
                         (rcompose/comp f-comp g-comp)))))

;; ===================================================================================================
;; Pairs

(: pair/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((pair/fwd fst-fwd snd-fwd) ω z γ)
  (let ([k1  (fst-fwd ω z γ)]
        [k2  (snd-fwd ω z γ)])
    (cons k1 k2)))

(: pair/pre (Preimage-Fun Preimage-Fun
                          Omega-Rect Omega-Rect Omega-Rect Nonempty-Set
                          Maybe-Branches-Rect Maybe-Branches-Rect Set Set
                          -> Simple-Preimage-Fun))
(define (pair/pre pre1 pre2 Ω Ω1 Ω2 Γ old-Z1 old-Z2 old-K1 old-K2)
  (λ (Z K)
    (define K1 (set-pair-ref K 'fst))
    (define K2 (set-pair-ref K 'snd))
    (define Z1 (branches-rect-fst Z))
    (define Z2 (branches-rect-snd Z))
    (define 1? (if pair-cache-preimages? (not (and (eq? K1 old-K1) (eq? Z1 old-Z1))) #t))
    (define 2? (if pair-cache-preimages? (not (and (eq? K2 old-K2) (eq? Z2 old-Z2))) #t))
    (when (and pair-cache-preimages? pair-preimage-stats?)
      (increment-cache-stat (if 1? 'pair-preimage-hits/fst 'pair-preimage-misses/fst))
      (increment-cache-stat (if 2? 'pair-preimage-hits/snd 'pair-preimage-misses/snd)))
    (let*-values ([(Ω1 Z1 Γ1)  (if 1? (pre1 Z1 K1) (values Ω1 Z1 Γ))]
                  [(Ω2 Z2 Γ2)  (if 2? (pre2 Z2 K2) (values Ω2 Z2 Γ))]
                  [(Ω)  (unit-omega-rect-node/last Ω Ω1 Ω2)]
                  [(Z)  (branches-rect-node/last Z booleans Z1 Z2)]
                  [(Γ)  (cond [(eq? Γ1 Γ)  (set-intersect Γ Γ2)]
                              [(eq? Γ2 Γ)  (set-intersect Γ Γ1)]
                              [else  (set-intersect Γ1 Γ2)])])
      (values Ω Z Γ))))

(: pair/comp (Computation Computation -> Computation))
(define (pair/comp comp1 comp2)
  (cached-computation
   (λ (Ω Z Γ)
     (define Ω1 (omega-rect-fst Ω))
     (define Ω2 (omega-rect-snd Ω))
     (define Z1 (branches-rect-fst Z))
     (define Z2 (branches-rect-fst Z))
     (match-let* ([(computation-meaning Z1 K1 pre1)  (comp1 Ω1 Z1 Γ)]
                  [(computation-meaning Z2 K2 pre2)  (comp2 Ω2 Z2 Γ)]
                  [Z  (branches-rect-node/last Z booleans Z1 Z2)]
                  [K  (set-pair K1 K2)])
       (define pre (simple-preimage Ω Γ Z K (pair/pre pre1 pre2 Ω Ω1 Ω2 Γ Z1 Z2 K1 K2)))
       (computation-meaning Z K pre)))))

(: pair/arr (expression expression -> expression))
(define (pair/arr expr1 expr2)
  (expression
   (λ (r)
     (match-define (expression-meaning idxs1 fwd1 comp1) (run-expression expr1 (cons 0 r)))
     (match-define (expression-meaning idxs2 fwd2 comp2) (run-expression expr2 (cons 1 r)))
     (expression-meaning (append idxs1 idxs2)
                         (pair/fwd fwd1 fwd2)
                         (pair/comp comp1 comp2)))))

;; ===================================================================================================
;; Conditionals

(: switch/fwd (Omega-Index (-> Forward-Fun) (-> Forward-Fun) -> Forward-Fun))
(define ((switch/fwd idx t-fwd f-fwd) ω z γ)
  (match-let ([(cons b γ)  γ])
    (define zb (branches-ref z idx))
    (cond [(not (boolean? b))  (raise-argument-error 'switch/fwd "Boolean" b)]
          [(and (eq? b #t) (or (eq? zb #t) (eq? zb 'either)))  ((t-fwd) ω z γ)]
          [(and (eq? b #f) (or (eq? zb #f) (eq? zb 'either)))  ((f-fwd) ω z γ)]
          [else  (raise (forward-fail "bad branch"))])))

(: switch-t/pre ((-> Computation) Omega-Rect Omega-Rect Omega-Rect Nonempty-Set
                                     -> Simple-Preimage-Fun))
(define ((switch-t/pre comp Ω Ωt Ωf Γ) Z K)
  (define Zt (branches-rect-fst Z))
  (define Zf (branches-rect-snd Z))
  (match-let ([(computation-meaning Zt Kt pre)  ((comp) Ωt Zt Γ)])
    (let*-values ([(Ωt Zt Γ)  (pre Zt (set-intersect K Kt))]
                  [(Ω)  (unit-omega-rect-node/last Ω Ωt Ωf)]
                  [(Z)  (branches-rect-node/last Z trues Zt Zf)]
                  [(Γ)  (set-pair trues Γ)])
      (values Ω Z Γ))))

(: switch-f/pre ((-> Computation) Omega-Rect Omega-Rect Omega-Rect Nonempty-Set
                                      -> Simple-Preimage-Fun))
(define ((switch-f/pre comp Ω Ωt Ωf Γ) Z K)
  (define Zt (branches-rect-fst Z))
  (define Zf (branches-rect-snd Z))
  (match-let ([(computation-meaning Zf Kf pre)  ((comp) Ωf Zf Γ)])
    (let*-values ([(Ωf Zf Γ)  (pre Zf (set-intersect K Kf))]
                  [(Ω)  (unit-omega-rect-node/last Ω Ωt Ωf)]
                  [(Z)  (branches-rect-node/last Z falses Zt Zf)]
                  [(Γ)  (set-pair falses Γ)])
      (values Ω Z Γ))))

(: switch-tf/pre ((-> Computation) (-> Computation) Omega-Rect Omega-Rect Omega-Rect
                                        Nonempty-Set Nonempty-Set -> Simple-Preimage-Fun))
(define ((switch-tf/pre t-comp f-comp Ω Ωt Ωf Γ Γtf) Z K)
  (define b (branches-rect-value Z))
  (cond [(eq? b booleans)  (values Ω Z Γ)]
        [(eq? b trues)     ((switch-t/pre t-comp Ω Ωt Ωf Γtf) Z K)]
        [(eq? b falses)    ((switch-f/pre f-comp Ω Ωt Ωf Γtf) Z K)]
        [else  (values empty-set empty-set empty-set)]))

(: switch/comp ((-> Computation) (-> Computation) -> Computation))
(define (switch/comp t-comp f-comp)
  (cached-computation
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
           (define pre (simple-preimage Ω Γ Z K (switch-tf/pre t-comp f-comp Ω Ωt Ωf Γ Γtf)))
           (computation-meaning Z K pre)]
          [(eq? Γb trues)
           (match-let* ([(computation-meaning Zt Kt pre)  ((t-comp) Ωt Zt Γtf)]
                        [Z  (branches-rect-node/last Z trues Zt Zf)])
             (define pre (simple-preimage Ω Γ Z Kt (switch-t/pre t-comp Ω Ωt Ωf Γtf)))
             (computation-meaning Z Kt pre))]
          [(eq? Γb falses)
           (match-let* ([(computation-meaning Zf Kf pre)  ((f-comp) Ωf Zf Γtf)]
                        [Z  (branches-rect-node/last Z falses Zt Zf)])
             (define pre (simple-preimage Ω Γ Z Kf (switch-f/pre f-comp Ω Ωt Ωf Γtf)))
             (computation-meaning Z Kf pre))]
          [else
           ;; Shouldn't be possible
           (raise-argument-error 'switch/comp "Boolean-Rect" Γb)])]
       [_
        (bottom/comp empty-set empty-set empty-set)]))))

(: switch/arr (expression expression -> expression))
(define (switch/arr t-expr f-expr)
  (expression
   (λ (r)
     (define idx (reverse r))
     (define t-meaning (delay (run-expression t-expr (cons 0 r))))
     (define f-meaning (delay (run-expression f-expr (cons 1 r))))
     (expression-meaning
      (list (if-indexes idx
                        (λ () (expression-meaning-indexes (force t-meaning)))
                        (λ () (expression-meaning-indexes (force f-meaning)))))
      (switch/fwd idx
                  (λ () (expression-meaning-forward (force t-meaning)))
                  (λ () (expression-meaning-forward (force f-meaning))))
      (switch/comp (λ () (expression-meaning-computation (force t-meaning)))
                   (λ () (expression-meaning-computation (force f-meaning))))))))

(: lazy-if/arr (expression expression expression -> expression))
(define (lazy-if/arr c-expr t-expr f-expr)
  (rcompose/arr (pair/arr c-expr id/arr)
                (switch/arr t-expr f-expr)))

;; ===================================================================================================
;; Random

(: random/fwd (Omega-Index -> Forward-Fun))
(define ((random/fwd idx) ω z γ)
  (omega-ref ω idx))

(: random/pre (Omega-Rect Nonempty-Set -> Simple-Preimage-Fun))
(define (random/pre Ω Γ)
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (Z K)
    (cond [(interval? K)  (values (omega-rect-node K Ω1 Ω2) Z Γ)]
          [else           (values empty-set empty-set empty-set)])))

(: random/comp (-> Computation))
(define (random/comp)
  (cached-computation
   (λ (Ω Z Γ)
     (define K (omega-rect-value Ω))
     (computation-meaning Z K (simple-preimage Ω Γ Z K (random/pre Ω Γ))))))

(: random/arr expression)
(define random/arr
  (expression
   (λ (r)
     (define idx (reverse r))
     (expression-meaning (list (make-interval-index idx interval-split))
                         (random/fwd idx)
                         (random/comp)))))

;; ===================================================================================================
;; Random boolean

(: boolean/fwd (Omega-Index Flonum -> Forward-Fun))
(define ((boolean/fwd r p) ω z γ)
  ((omega-ref ω r) . < . p))

(: boolean/pre (Omega-Rect Nonempty-Set (U Empty-Set Interval) (U Empty-Set Interval)
                           -> Simple-Preimage-Fun))
(define (boolean/pre Ω Γ It If)
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (Z K)
    (define I (cond [(eq? K booleans)  (set-join It If)]
                    [(eq? K trues)     It]
                    [(eq? K falses)    If]
                    [else              empty-set]))
    (cond [(interval? I)  (values (omega-rect-node I Ω1 Ω2) Z Γ)]
          [else           (values empty-set empty-set empty-set)])))

(: boolean/comp (Interval Interval -> Computation))
(define (boolean/comp It If)
  (cached-computation
   (λ (Ω Z Γ)
     (define I (omega-rect-value Ω))
     (let ([It  (interval-intersect I It)]
           [If  (interval-intersect I If)])
       (define K (booleans->boolean-rect (not (empty-set? It)) (not (empty-set? If))))
       (define pre (simple-preimage Ω Γ Z K (boolean/pre Ω Γ It If)))
       (computation-meaning Z K pre)))))

(: boolean/arr (Flonum -> expression))
(define (boolean/arr p)
  (cond
    [(and (p . > . 0.0) (p . < . 1.0))
     (define It (Interval 0.0 p #t #f))
     (define If (Interval p 1.0 #t #t))
     (define split (make-constant-splitter (list It If)))
     (expression
      (λ (r)
        (define idx (reverse r))
        (expression-meaning (list (interval-index idx split 1 0.0))
                            (boolean/fwd idx p)
                            (boolean/comp It If))))]
    [(= p 0.0)  (c/arr #f)]
    [(= p 1.0)  (c/arr #t)]
    [else  (raise-argument-error 'boolean "probability" p)]))

;; ===================================================================================================
;; Primitive conditional

(: prim-if/fwd (Forward-Fun Forward-Fun Forward-Fun -> Forward-Fun))
(define ((prim-if/fwd c-fwd t-fwd f-fwd) ω z γ)
  (define kc (c-fwd ω z γ))
  (cond [(eq? kc #t)  (t-fwd ω z γ)]
        [(eq? kc #f)  (f-fwd ω z γ)]
        [else  (raise-argument-error 'prim-if/fwd "Boolean" kc)]))

(: prim-if-one/pre (Nonempty-Set Preimage-Fun -> Prim-Preimage-Fun))
(define ((prim-if-one/pre orig-K pre) K)
  (run-prim-preimage pre (set-intersect orig-K K)))

(: prim-if-both/pre (Nonempty-Set Nonempty-Set Preimage-Fun Preimage-Fun -> Prim-Preimage-Fun))
(define ((prim-if-both/pre Kt Kf t-pre f-pre) K)
  (set-join (run-prim-preimage t-pre (set-intersect Kt K))
            (run-prim-preimage f-pre (set-intersect Kf K))))

(: prim-if/comp (Computation Computation Computation -> Computation))
(define (prim-if/comp c-comp t-comp f-comp)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define-values (Kc c-pre) (run-prim-computation c-comp Γ))
     (define Γt (if (set-member? Kc #t) (run-prim-preimage c-pre trues) empty-set))
     (define Γf (if (set-member? Kc #f) (run-prim-preimage c-pre falses) empty-set))
     
     (define-values (K pre)
       (cond
         [(and (empty-set? Γt) (empty-set? Γf))  (values empty-set id/pre)]
         [(empty-set? Γf)
          (define-values (Kt t-pre) (run-prim-computation t-comp Γt))
          (cond [(empty-set? Kt)  (values empty-set id/pre)]
                [else  (values Kt (prim-if-one/pre Kt t-pre))])]
         [(empty-set? Γt)
          (define-values (Kf f-pre) (run-prim-computation f-comp Γf))
          (cond [(empty-set? Kf)  (values empty-set id/pre)]
                [else  (values Kf (prim-if-one/pre Kf f-pre))])]
         [else
          (define-values (Kt t-pre) (run-prim-computation t-comp Γt))
          (define-values (Kf f-pre) (run-prim-computation f-comp Γf))
          (cond [(and (empty-set? Kt) (empty-set? Kf))  (values empty-set id/pre)]
                [(empty-set? Kf)  (values Kt (prim-if-one/pre Kt t-pre))]
                [(empty-set? Kt)  (values Kf (prim-if-one/pre Kf f-pre))]
                [else  (values (set-join Kt Kf) (prim-if-both/pre Kt Kf t-pre f-pre))])]))
     
     (computation-meaning branches-rect K (prim-preimage (pre universe) K pre)))))

(: prim-if/arr (expression expression expression -> expression))
(define (prim-if/arr c-expr t-expr f-expr)
  (expression
   (λ (r)
     (define idx (reverse r))
     (match-define (expression-meaning c-idxs c-fwd c-comp) (run-expression c-expr (cons 0 r)))
     (match-define (expression-meaning t-idxs t-fwd t-comp) (run-expression t-expr (list* 0 1 r)))
     (match-define (expression-meaning f-idxs f-fwd f-comp) (run-expression f-expr (list* 1 1 r)))
     (expression-meaning (append c-idxs t-idxs f-idxs)
                         (prim-if/fwd c-fwd t-fwd f-fwd)
                         (prim-if/comp c-comp t-comp f-comp)))))

;; ===================================================================================================
;; Ref

(: ref/fwd (Pair-Index -> Forward-Fun))
(define ((ref/fwd j) ω z γ)
  (value-pair-ref γ j))

(: ref/pre (Nonempty-Set Pair-Index -> Prim-Preimage-Fun))
(define ((ref/pre Γ j) K) (set-pair-set Γ j K))

(: ref/comp (Pair-Index -> Computation))
(define (ref/comp j)
  (cached-prim-computation
   all-pairs
   (λ (Γ)
     (define K (set-pair-ref Γ j))
     (computation-meaning branches-rect K (prim-preimage Γ K (ref/pre Γ j))))))

(: ref/arr (Pair-Index -> expression))
(define (ref/arr j)
  (define fwd (ref/fwd j))
  (expression (λ (r) (expression-meaning empty fwd (ref/comp j)))))

;; ===================================================================================================
;; Monotone R -> R functions

(: monotone/fwd (Symbol (Flonum -> Flonum) -> Forward-Fun))
(define ((monotone/fwd name f) ω z γ)
  (cond [(flonum? γ)  (f γ)]
        [else  (raise-argument-error name "Flonum" γ)]))

(: monotone-range (Interval Interval (Flonum -> Flonum) Boolean -> Set))
(define (monotone-range domain f-range f fx?)
  (match-define (interval a b a? b?) domain)
  (cond [fx?   (set-intersect f-range (interval (f a) (f b) a? b?))]
        [else  (set-intersect f-range (interval (f b) (f a) b? a?))]))

(: monotone/pre (Nonempty-Set (Flonum -> Flonum) Boolean -> Prim-Preimage-Fun))
(define ((monotone/pre Γ g fx?) K)
  (match K
    [(interval a b a? b?)
     (cond [fx?   (set-intersect Γ (interval (g a) (g b) a? b?))]
           [else  (set-intersect Γ (interval (g b) (g a) b? a?))])]
    [_  empty-set]))

(: monotone/comp (Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean -> Computation))
(define (monotone/comp f-domain f-range f g fx?)
  (cached-prim-computation
   f-domain
   (λ (Γ)
     (define K (monotone-range (assert Γ interval?) f-range f fx?))
     (computation-meaning branches-rect K (prim-preimage Γ K (monotone/pre Γ g fx?))))))

(: monotone/arr (Symbol Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                        -> expression))
(define (monotone/arr name f-domain f-range f g fx?)
  (expression
   (λ (r)
     (define fwd (monotone/fwd name f))
     (define comp (monotone/comp f-domain f-range f g fx?))
     (expression-meaning empty fwd comp))))

;; ===================================================================================================
;; Monotone R x R -> R functions

(: monotone2d/fwd (Symbol (Flonum Flonum -> Flonum) -> Forward-Fun))
(define ((monotone2d/fwd name f) ω z γ)
  (match γ
    [(cons (? flonum? x) (? flonum? y))  (f x y)]
    [_  (raise-argument-error name "(Pair Flonum Flonum)" γ)]))

(: monotone2d-range (Nonempty-Set Interval (Flonum Flonum -> Flonum) Boolean Boolean -> Set))
(define (monotone2d-range domain f-range f fx? fy?)
  (match-define (pair-rect (interval xa xb xa? xb?) (interval ya yb ya? yb?)) domain)
  (let-values ([(xa xb xa? xb?)  (if fx? (values xa xb xa? xb?) (values xb xa xb? xa?))]
               [(ya yb ya? yb?)  (if fy? (values ya yb ya? yb?) (values yb ya yb? ya?))])
    (set-intersect f-range (interval (f xa ya) (f xb yb) (and xa? ya?) (and xb? yb?)))))

(: monotone2d/pre (Nonempty-Set
                   (Flonum Flonum -> Flonum) Boolean Boolean
                   (Flonum Flonum -> Flonum) Boolean Boolean
                   -> Prim-Preimage-Fun))
(define (monotone2d/pre Γ g gz? gy? h hz? hx?)
  (match-define (pair-rect (interval xa xb xa? xb?) (interval ya yb ya? yb?)) Γ)
  (λ (K)
    (match K
      [(interval za zb za? zb?)
       (define X
         (let-values ([(za zb za? zb?)  (if gz? (values za zb za? zb?) (values zb za zb? za?))]
                      [(ya yb ya? yb?)  (if gy? (values ya yb ya? yb?) (values yb ya yb? ya?))])
           (interval (g za ya) (g zb yb) (and za? ya?) (and zb? yb?))))
       (define Y
         (let-values ([(za zb za? zb?)  (if hz? (values za zb za? zb?) (values zb za zb? za?))]
                      [(xa xb xa? xb?)  (if hx? (values xa xb xa? xb?) (values xb xa xb? xa?))])
           (interval (h za xa) (h zb xb) (and za? xa?) (and zb? xb?))))
       (set-intersect Γ (set-pair X Y))]
      [_
       empty-set])))

(: monotone2d/comp (Pair-Rect Interval
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              -> Computation))
(define (monotone2d/comp f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (cached-prim-computation
   f-domain
   (λ (Γ)
     (define K (monotone2d-range Γ f-range f fx? fy?))
     (define pre (prim-preimage Γ K (monotone2d/pre Γ g gz? gy? h hz? hx?)))
     (computation-meaning branches-rect K pre))))

(: monotone2d/arr (Symbol Pair-Rect Interval
                          (Flonum Flonum -> Flonum) Boolean Boolean
                          (Flonum Flonum -> Flonum) Boolean Boolean
                          (Flonum Flonum -> Flonum) Boolean Boolean
                          -> expression))
(define (monotone2d/arr name f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (expression
   (λ (r)
     (define fwd (monotone2d/fwd name f))
     (define comp (monotone2d/comp f-domain f-range f fx? fy? g gz? gy? h hz? hx?))
     (expression-meaning empty fwd comp))))

;; ===================================================================================================
;; Predicates

(: predicate/fwd ((Value -> Boolean) -> Forward-Fun))
(define ((predicate/fwd pred?) ω z γ)
  (pred? γ))

(: predicate-range (Set Set -> (U Empty-Set Boolean-Rect)))
(define (predicate-range true-set false-set)
  (booleans->boolean-rect (not (empty-set? true-set))
                          (not (empty-set? false-set))))

(: predicate/pre (Set Set -> Prim-Preimage-Fun))
(define ((predicate/pre true-set false-set) B)
  (cond [(eq? B booleans)  (set-join true-set false-set)]
        [(eq? B trues)     true-set]
        [(eq? B falses)    false-set]
        [else              empty-set]))

(: predicate/comp (Set Set -> Computation))
(define (predicate/comp true-set false-set)
  (cached-prim-computation
   (set-join true-set false-set)
   (λ (Γ)
     (let ([true-set   (set-intersect Γ true-set)]
           [false-set  (set-intersect Γ false-set)])
       (define K (predicate-range true-set false-set))
       (define pre (prim-preimage Γ K (predicate/pre true-set false-set)))
       (computation-meaning branches-rect K pre)))))

(: predicate/arr ((Value -> Boolean) Nonempty-Set Nonempty-Set -> expression))
(define (predicate/arr pred? true-set false-set)
  (define fwd (predicate/fwd pred?))
  (define comp (predicate/comp true-set false-set))
  (expression (λ (r) (expression-meaning empty fwd comp))))

;; ===================================================================================================
;; Tagged values

(: tag?/arr (Set-Tag -> expression))
(define (tag?/arr tag)
  (predicate/arr (λ: ([γ : Value]) (eq? tag (value-tag γ)))
                 (bot-set tag universe)
                 (top-set tag empty-set)))

;; ---------------------------------------------------------------------------------------------------

(: tag/fwd (Set-Tag -> Forward-Fun))
(define ((tag/fwd tag) ω z γ)
  (tagged tag γ))

(: tag/comp (Set-Tag -> Computation))
(define (tag/comp tag)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define K (set-tag Γ tag))
     (define pre (prim-preimage Γ K (λ (B) (set-untag B tag))))
     (computation-meaning branches-rect K pre))))

(: tag/arr (Set-Tag -> expression))
(define (tag/arr tag)
  (define fwd (tag/fwd tag))
  (define comp (tag/comp tag))
  (expression (λ (r) (expression-meaning empty fwd comp))))

;; ---------------------------------------------------------------------------------------------------

(: untag/fwd (Set-Tag -> Forward-Fun))
(define ((untag/fwd tag) ω z γ)
  (if (and (tagged? γ) (eq? tag (get-tag γ)))
      (get-val γ)
      (raise (forward-fail (format "expected ~e; given ~e" tag γ)))))

(: untag/comp (Set-Tag -> Computation))
(define (untag/comp tag)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define K (set-untag Γ tag))
     (define pre (prim-preimage Γ K (λ (B) (set-tag B tag))))
     (computation-meaning branches-rect K pre))))

(: untag/arr (Set-Tag -> expression))
(define (untag/arr tag)
  (define fwd (untag/fwd tag))
  (define comp (untag/comp tag))
  (expression (λ (r) (expression-meaning empty fwd comp))))
