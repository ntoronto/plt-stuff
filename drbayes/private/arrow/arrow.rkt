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

(define-type Forward-Fun (Omega Value -> (Values Value Maybe-Branches-Rect)))

(: run-expression (case-> (expression -> expression-meaning)
                          (expression Omega-Index -> expression-meaning)))
(define (run-expression e [r empty])
  ((expression-fun e) r))

;; A computation is a function from a domain and branches to its meaning
(define-type Computation (Maybe-Omega-Rect Set -> computation-meaning))

;; A computation means:
;;  1. A branches rectangle (which bounds the branches the forward computation can take)
;;  2. The approximate range of its forward function
;;  3. A function that computes approximate preimages under its forward function
(struct: computation-meaning ([range : Set]
                              [branches : Maybe-Branches-Rect]
                              [preimage : Preimage-Fun])
  #:transparent)

(define-type Preimage-Fun (Set Maybe-Branches-Rect -> (Values Maybe-Omega-Rect Set)))

;; ===================================================================================================
;; Convenience and caching wrappers

(define-type Simple-Preimage-Fun (Nonempty-Set Branches-Rect -> (Values Maybe-Omega-Rect Set)))
(define-type Prim-Preimage-Fun (Nonempty-Set -> Set))

(: simple-preimage (Maybe-Omega-Rect Set Set Maybe-Branches-Rect Simple-Preimage-Fun
                                     -> Preimage-Fun))
;; Wraps a Prim-Preimage-Fun with code that ensures the argument is a subset of the range and is
;; nonempty
(define ((simple-preimage Ω Γ K Z pre) Ksub Zsub)
  (let ([Ksub  (set-intersect K Ksub)]
        [Zsub  (branches-rect-intersect Z Zsub)])
    (cond [(or (empty-set? Ksub) (empty-set? Zsub))
           (values empty-set empty-set)]
          [(not cache-preimages?)
           (pre Ksub Zsub)]
          [(and (eq? Ksub K) (eq? Zsub Z))
           (when preimage-stats? (increment-cache-stat 'preimage-hits))
           (values Ω Γ)]
          [else
           (when preimage-stats?
             (increment-cache-stat 'preimage-misses)
             (when (and (not (eq? Ksub K)) (equal? Ksub K))
               (increment-cache-stat 'preimage-misses/bad-K))
             (when (and (not (eq? Zsub Z)) (equal? Zsub Z))
               (increment-cache-stat 'preimage-misses/bad-Z)))
           (pre Ksub Zsub)])))

(: prim-preimage (Set Set Prim-Preimage-Fun -> Preimage-Fun))
;; Like `simple-preimage' but for preimages under primitives
(define ((prim-preimage Γ K pre) Ksub Zsub)
  (let ([Ksub  (set-intersect K Ksub)])
    (cond [(empty-set? Ksub)
           (values empty-set empty-set)]
          [(not prim-cache-preimages?)
           (values omega-rect (pre Ksub))]
          [(eq? Ksub K)
           (when prim-preimage-stats? (increment-cache-stat 'prim-preimage-hits))
           (values omega-rect Γ)]
          [else
           (when prim-preimage-stats?
             (increment-cache-stat 'prim-preimage-misses)
             (when (and (not (eq? Ksub K)) (equal? Ksub K))
               (increment-cache-stat 'prim-preimage-misses/bad-K)))
           (values omega-rect (pre Ksub))])))

(define-type Simple-Computation (Omega-Rect Nonempty-Set -> computation-meaning))
(define-type Prim-Computation (Nonempty-Set -> computation-meaning))

(: cached-computation (Simple-Computation -> Computation))
(define (cached-computation comp)
  (define: last-Ω : (U #f Omega-Rect)  #f)
  (define: last-Γ : (U #f Nonempty-Set)  #f)
  (define: last-m : (U #f computation-meaning)  #f)
  (λ (Ω Γ)
    (define cached-m last-m)
    (cond
      [(or (empty-set? Ω) (empty-set? Γ))  (bottom/comp Ω Γ)]
      [(not cache-computations?)  (comp Ω Γ)]
      [(and (eq? Ω last-Ω) (eq? Γ last-Γ) cached-m)
       (when computation-stats? (increment-cache-stat 'computation-hits))
       cached-m]
      [else
       (when computation-stats?
         (increment-cache-stat 'computation-misses)
         (when (and (not (eq? Ω last-Ω)) (equal? Ω last-Ω))
           (increment-cache-stat 'computation-misses/bad-Ω))
         (when (and (not (eq? Γ last-Γ)) (equal? Γ last-Γ))
           (increment-cache-stat 'computation-misses/bad-Γ)))
       (set! last-Ω Ω)
       (set! last-Γ Γ)
       (let ([cached-m  (comp Ω Γ)])
         (set! last-m cached-m)
         cached-m)])))

(: cached-prim-computation (Set Prim-Computation -> Computation))
(define (cached-prim-computation domain comp)
  (define: last-Γ : (U #f Nonempty-Set)  #f)
  (define: last-m : (U #f computation-meaning)  #f)
  (λ (Ω orig-Γ)
    (define Γ (set-intersect orig-Γ domain))
    (define cached-m last-m)
    (cond
      [(or (empty-set? Ω) (empty-set? Γ))  (bottom/comp Ω Γ)]
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
         cached-m)])))

;; ===================================================================================================
;; Basic primitives

;; Bottom function: has empty range

(: bottom/fwd Forward-Fun)
(define (bottom/fwd ω γ)
  (error 'bottom/fwd "empty range"))

(: bottom/comp Computation)
(define (bottom/comp _Ω _Γ)
  (computation-meaning empty-set empty-set (λ (Ksub Zsub) (values empty-set empty-set))))

(define bottom/arr
  (expression (λ (r) (expression-meaning empty bottom/fwd bottom/comp))))

;; Top function: has universal range

(: top/fwd Forward-Fun)
(define (top/fwd ω γ)
  (error 'top/fwd "universal range"))

(: top/comp (-> Computation))
(define (top/comp)
  (cached-computation
   (λ (Ω Γ)
     (computation-meaning universe branches-rect (λ (Ksub Zsub) (values Ω Γ))))))

(define top/arr
  (expression (λ (r) (expression-meaning empty top/fwd (top/comp)))))

;; Identity function

(: id/fwd Forward-Fun)
(define (id/fwd ω γ) (values γ branches-rect))

(: id/comp (-> Computation))
(define (id/comp)
  (cached-prim-computation
   universe
   (λ (Γ) (computation-meaning Γ branches-rect (prim-preimage Γ Γ (λ (B) B))))))

(define id/arr
  (expression (λ (r) (expression-meaning empty id/fwd (id/comp)))))

;; Constant functions

(: c/fwd (Value -> Forward-Fun))
(define ((c/fwd x) ω γ) (values x branches-rect))

(: c/comp (Nonempty-Set Nonempty-Set -> Computation))
(define (c/comp domain X)
  (cached-prim-computation
   domain
   (λ (Γ)
     (define pre (prim-preimage Γ X (λ (B) (if (empty-set? B) empty-set Γ))))
     (computation-meaning X branches-rect pre))))

(: c/arr (case-> (Value -> expression)
                 (Value Nonempty-Set -> expression)))
(define (c/arr x [domain universe])
  (define fwd (c/fwd x))
  (define X (value->singleton x))
  (expression (λ (r) (expression-meaning empty fwd (c/comp domain X)))))

;; ===================================================================================================
;; Arrow composition (reverse composition)

(: rcompose/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((rcompose/fwd f-fwd g-fwd) ω γ)
  (define-values (kf zf) (f-fwd ω γ))
  (define-values (kg zg) (g-fwd ω kf))
  (values kg (branches-rect-node 'tf zf zg)))

(: rcompose/pre (Preimage-Fun Preimage-Fun Omega-Rect -> Simple-Preimage-Fun))
(define ((rcompose/pre f-pre g-pre Ω) Kg Z)
  (define-values (Ωg Γg) (g-pre Kg (branches-rect-snd Z)))
  (define-values (Ωf Γf) (f-pre Γg (branches-rect-fst Z)))
  (values (unit-omega-rect-node/last Ω Ωf Ωg) Γf))

(: rcompose/comp (Computation Computation -> Computation))
(define (rcompose/comp f-comp g-comp)
  (cached-computation
   (λ (Ω Γf)
     (match-define (computation-meaning Kf Zf f-pre) (f-comp (omega-rect-fst Ω) Γf))
     (match-define (computation-meaning Kg Zg g-pre) (g-comp (omega-rect-snd Ω) Kf))
     (define Z (branches-rect-node 'tf Zf Zg))
     (define pre (simple-preimage Ω Γf Kg Z (rcompose/pre f-pre g-pre Ω)))
     (computation-meaning Kg Z pre))))

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
(define ((pair/fwd fst-fwd snd-fwd) ω γ)
  (define-values (k1 z1) (fst-fwd ω γ))
  (define-values (k2 z2) (snd-fwd ω γ))
  (values (cons k1 k2) (branches-rect-node 'tf z1 z2)))

(: pair/pre (Preimage-Fun Preimage-Fun
                          Omega-Rect Omega-Rect Omega-Rect Nonempty-Set
                          Set Set Maybe-Branches-Rect Maybe-Branches-Rect
                          -> Simple-Preimage-Fun))
(define (pair/pre pre1 pre2 Ω old-Ω1 old-Ω2 Γ old-K1 old-K2 old-Z1 old-Z2)
  (λ (K Z)
    (define K1 (set-pair-ref K 'fst))
    (define K2 (set-pair-ref K 'snd))
    (define Z1 (branches-rect-fst Z))
    (define Z2 (branches-rect-snd Z))
    (define 1? (if pair-cache-preimages? (not (and (eq? K1 old-K1) (eq? Z1 old-Z1))) #t))
    (define 2? (if pair-cache-preimages? (not (and (eq? K2 old-K2) (eq? Z2 old-Z2))) #t))
    (when (and pair-cache-preimages? pair-preimage-stats?)
      (increment-cache-stat (if 1? 'pair-preimage-hits/fst 'pair-preimage-misses/fst))
      (increment-cache-stat (if 2? 'pair-preimage-hits/snd 'pair-preimage-misses/snd)))
    (let-values ([(Ω1 Γ1)  (if 1? (pre1 K1 Z1) (values old-Ω1 Γ))]
                 [(Ω2 Γ2)  (if 2? (pre2 K2 Z2) (values old-Ω2 Γ))])
      (values (unit-omega-rect-node/last Ω Ω1 Ω2)
              (cond [(eq? Γ1 Γ)  (set-intersect Γ Γ2)]
                    [(eq? Γ2 Γ)  (set-intersect Γ Γ1)]
                    [else  (set-intersect Γ1 Γ2)])))))

(: pair/comp (Computation Computation -> Computation))
(define (pair/comp comp1 comp2)
  (cached-computation
   (λ (Ω Γ)
     (define Ω1 (omega-rect-fst Ω))
     (define Ω2 (omega-rect-snd Ω))
     (match-define (computation-meaning K1 Z1 pre1) (comp1 Ω1 Γ))
     (match-define (computation-meaning K2 Z2 pre2) (comp2 Ω2 Γ))
     (define K (set-pair K1 K2))
     (define Z (branches-rect-node 'tf Z1 Z2))
     (define pre (simple-preimage Ω Γ K Z (pair/pre pre1 pre2 Ω Ω1 Ω2 Γ K1 K2 Z1 Z2)))
     (computation-meaning K Z pre))))

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

(define-predicate if-bad-branch? 'if-bad-branch)

(: switch/fwd (Omega-Index (-> Forward-Fun) (-> Forward-Fun) -> Forward-Fun))
(define ((switch/fwd idx t-fwd f-fwd) ω γ)
  (match γ
    [(cons #t γ)  (define-values (kt zt) ((t-fwd) ω γ))
                  (values kt (branches-rect-node 't zt branches-rect))]
    [(cons #f γ)  (define-values (kf zf) ((f-fwd) ω γ))
                  (values kf (branches-rect-node 'f branches-rect zf))]
    [_  (raise-argument-error 'switch/fwd "(Pair Boolean Value)" γ)]))

(: switch/comp ((-> Computation) (-> Computation) -> Computation))
(define (switch/comp t-comp f-comp)
  (cached-computation
   (λ (Ω orig-Γ)
     (define Γ (set-intersect orig-Γ (set-pair booleans universe)))
     (cond
       [(empty-set? Γ)  (bottom/comp empty-set empty-set)]
       [else
        (define Γb (set-pair-ref Γ 'fst))
        (define Γtf (set-pair-ref Γ 'snd))
        (define Ωt (omega-rect-fst Ω))
        (define Ωf (omega-rect-snd Ω))
        
        (define t-meaning (delay ((t-comp) Ωt Γtf)))
        (define f-meaning (delay ((f-comp) Ωf Γtf)))
        
        (: t/pre Simple-Preimage-Fun)
        (define (t/pre K Z)
          (match-define (computation-meaning Kt Zt pre) (force t-meaning))
          (let-values ([(Ωt Γt)  (pre (set-intersect Kt K)
                                      (branches-rect-intersect Zt (branches-rect-fst Z)))])
            (values (unit-omega-rect-node/last Ω Ωt Ωf)
                    (set-pair trues Γt))))
        
        (: f/pre Simple-Preimage-Fun)
        (define (f/pre K Z)
          (match-define (computation-meaning Kf Zf pre) (force f-meaning))
          (let-values ([(Ωf Γf)  (pre (set-intersect Kf K)
                                      (branches-rect-intersect Zf (branches-rect-snd Z)))])
            (values (unit-omega-rect-node/last Ω Ωt Ωf)
                    (set-pair falses Γf))))
        
        (: switch/pre Simple-Preimage-Fun)
        (define (switch/pre K Z)
          (define b (branches-rect-value Z))
          (cond [(eq? b 'tf)  (values Ω Γ)]
                [(eq? b 't)   (t/pre K Z)]
                [(eq? b 'f)   (f/pre K Z)]
                [else  (values empty-set empty-set)]))
        
        (cond
          [(eq? Γb 'tf)
           (define K universe)
           (define Z branches-rect)
           (computation-meaning K Z (simple-preimage Ω Γ K Z switch/pre))]
          [(eq? Γb 't)
           (match-define (computation-meaning Kt Zt pre) (force t-meaning))
           (define Z (branches-rect-node 't Zt branches-rect))
           (computation-meaning Kt Z (simple-preimage Ω Γ Kt Z t/pre))]
          [(eq? Γb 'f)
           (match-define (computation-meaning Kf Zf pre) (force f-meaning))
           (define Z (branches-rect-node 'f branches-rect Zf))
           (computation-meaning Kf Z (simple-preimage Ω Γ Kf Z f/pre))]
          [else
           (bottom/comp empty-set empty-set)])]))))

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

(define strict-if/arr lazy-if/arr)

;; ===================================================================================================
;; Random

(: random/fwd (Omega-Index -> Forward-Fun))
(define ((random/fwd idx) ω γ)
  (values (omega-ref ω idx) branches-rect))

(: random/pre (Omega-Rect Nonempty-Set -> Simple-Preimage-Fun))
(define (random/pre Ω Γ)
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (K Z)
    (cond [(interval? K)  (values (omega-rect-node K Ω1 Ω2) Γ)]
          [else           (values empty-set empty-set)])))

(: random/comp (-> Computation))
(define (random/comp)
  (cached-computation
   (λ (Ω Γ)
     (define K (omega-rect-value Ω))
     (define Z branches-rect)
     (computation-meaning K Z (simple-preimage Ω Γ K Z (random/pre Ω Γ))))))

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
(define ((boolean/fwd r p) ω γ)
  (values ((omega-ref ω r) . < . p) branches-rect))

(: boolean/pre (Omega-Rect Nonempty-Set (U Empty-Set Interval) (U Empty-Set Interval)
                           -> Simple-Preimage-Fun))
(define (boolean/pre Ω Γ It If)
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (K Z)
    (define I (case K
                [(tf)  (set-join It If)]
                [(t)   It]
                [(f)   If]
                [else  empty-set]))
    (cond [(interval? I)  (values (omega-rect-node I Ω1 Ω2) Γ)]
          [else           (values empty-set empty-set)])))

(: boolean/comp (Interval Interval -> Computation))
(define (boolean/comp It If)
  (cached-computation
   (λ (Ω Γ)
     (define I (omega-rect-value Ω))
     (let ([It  (interval-intersect I It)]
           [If  (interval-intersect I If)])
       (define K (booleans->boolean-rect (not (empty-set? It)) (not (empty-set? If))))
       (define Z branches-rect)
       (define pre (simple-preimage Ω Γ K Z (boolean/pre Ω Γ It If)))
       (computation-meaning K Z pre)))))

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
(define ((prim-if/fwd c-fwd t-fwd f-fwd) ω γ)
  (define-values (kc zc) (c-fwd ω γ))
  (cond [(eq? kc #t)  (define-values (kt zt) (t-fwd ω γ))
                      (values kt branches-rect)]
        [(eq? kc #f)  (define-values (kf zf) (f-fwd ω γ))
                      (values kf branches-rect)]
        [else  (raise-argument-error 'prim-if/fwd "Boolean" kc)]))

(: prim-if/comp (Computation Computation Computation -> Computation))
(define (prim-if/comp c-comp t-comp f-comp)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define Ω omega-rect)
     (define Z branches-rect)
     
     (match-define (computation-meaning Kc Zc c-pre) (c-comp Ω Γ))
     (define-values (_Ωct Γt)
       (cond [(set-member? Kc #t)  (c-pre trues Z)]
             [else  (values empty-set empty-set)]))
     
     (define-values (_Ωcf Γf)
       (cond [(set-member? Kc #f)  (c-pre falses Z)]
             [else  (values empty-set empty-set)]))
     
     (define t? (not (empty-set? Γt)))
     (define f? (not (empty-set? Γf)))
     (cond
       [(and t? f?)
        (match-define (computation-meaning Kt Zt t-pre) (t-comp Ω Γt))
        (match-define (computation-meaning Kf Zf f-pre) (f-comp Ω Γf))
        (define K (set-join Kt Kf))
        
        (: prim-if/pre Prim-Preimage-Fun)
        (define (prim-if/pre K)
          (let-values ([(_Ωt Γt)  (t-pre (set-intersect Kt K) Z)]
                       [(_Ωf Γf)  (f-pre (set-intersect Kf K) Z)])
            (set-join Γt Γf)))
        
        (let-values ([(_Ωt Γt)  (t-pre Kt branches-rect)]
                     [(_Ωf Γf)  (f-pre Kf branches-rect)])
          (define Γ (set-join Γt Γf))
          (computation-meaning K Z (prim-preimage Γ K prim-if/pre)))]
       [t?
        (match-define (computation-meaning Kt Zt t-pre) (t-comp Ω Γt))
        (define K Kt)
        
        (: prim-if/pre Prim-Preimage-Fun)
        (define (prim-if/pre K)
          (let-values ([(_Ωt Γt)  (t-pre (set-intersect Kt K) Z)])
            Γt))
        
        (let-values ([(_Ωt Γt)  (t-pre Kt branches-rect)])
          (define Γ Γt)
          (computation-meaning K Z (prim-preimage Γ K prim-if/pre)))]
       [f?
        (match-define (computation-meaning Kf Zf f-pre) (f-comp Ω Γf))
        (define K Kf)
        
        (: prim-if/pre Prim-Preimage-Fun)
        (define (prim-if/pre K)
          (let-values ([(_Ωf Γf)  (f-pre (set-intersect Kf K) Z)])
            Γf))
        
        (let-values ([(_Ωf Γf)  (f-pre Kf branches-rect)])
          (define Γ Γf)
          (computation-meaning K Z (prim-preimage Γ K prim-if/pre)))]
       [else
        (bottom/comp empty-set empty-set)]))))

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
(define ((ref/fwd j) ω γ)
  (values (value-pair-ref γ j) branches-rect))

(: ref/pre (Nonempty-Set Pair-Index -> Prim-Preimage-Fun))
(define ((ref/pre Γ j) K) (set-pair-set Γ j K))

(: ref/comp (Pair-Index -> Computation))
(define (ref/comp j)
  (cached-prim-computation
   all-pairs
   (λ (Γ)
     (define K (set-pair-ref Γ j))
     (computation-meaning K branches-rect (prim-preimage Γ K (ref/pre Γ j))))))

(: ref/arr (Pair-Index -> expression))
(define (ref/arr j)
  (define fwd (ref/fwd j))
  (expression (λ (r) (expression-meaning empty fwd (ref/comp j)))))

;; ===================================================================================================
;; Monotone R -> R functions

(: monotone/fwd (Symbol (Flonum -> Flonum) -> Forward-Fun))
(define ((monotone/fwd name f) ω γ)
  (cond [(flonum? γ)  (values (f γ) branches-rect)]
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
     (computation-meaning K branches-rect (prim-preimage Γ K (monotone/pre Γ g fx?))))))

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
(define ((monotone2d/fwd name f) ω γ)
  (match γ
    [(cons (? flonum? x) (? flonum? y))  (values (f x y) branches-rect)]
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
     (computation-meaning K branches-rect pre))))

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
(define ((predicate/fwd pred?) ω γ)
  (values (pred? γ) branches-rect))

(: predicate-range (Set Set -> (U Empty-Set Boolean-Rect)))
(define (predicate-range true-set false-set)
  (booleans->boolean-rect (not (empty-set? true-set))
                          (not (empty-set? false-set))))

(: predicate/pre (Set Set -> Prim-Preimage-Fun))
(define ((predicate/pre true-set false-set) B)
  (case B
    [(tf)  (set-join true-set false-set)]
    [(t)   true-set]
    [(f)   false-set]
    [else  empty-set]))

(: predicate/comp (Set Set -> Computation))
(define (predicate/comp true-set false-set)
  (cached-prim-computation
   (set-join true-set false-set)
   (λ (Γ)
     (let ([true-set   (set-intersect Γ true-set)]
           [false-set  (set-intersect Γ false-set)])
       (define K (predicate-range true-set false-set))
       (define pre (prim-preimage Γ K (predicate/pre true-set false-set)))
       (computation-meaning K branches-rect pre)))))

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
(define ((tag/fwd tag) ω γ)
  (values (tagged tag γ) branches-rect))

(: tag/comp (Set-Tag -> Computation))
(define (tag/comp tag)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define K (set-tag Γ tag))
     (define pre (prim-preimage Γ K (λ (B) (set-untag B tag))))
     (computation-meaning K branches-rect pre))))

(: tag/arr (Set-Tag -> expression))
(define (tag/arr tag)
  (define fwd (tag/fwd tag))
  (define comp (tag/comp tag))
  (expression (λ (r) (expression-meaning empty fwd comp))))

;; ---------------------------------------------------------------------------------------------------

(: untag/fwd (Set-Tag -> Forward-Fun))
(define ((untag/fwd tag) ω γ)
  (if (and (tagged? γ) (eq? tag (get-tag γ)))
      (values (get-val γ) branches-rect)
      (raise-argument-error 'untag/fwd (symbol->string tag) γ)))

(: untag/comp (Set-Tag -> Computation))
(define (untag/comp tag)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define K (set-untag Γ tag))
     (define pre (prim-preimage Γ K (λ (B) (set-tag B tag))))
     (computation-meaning K branches-rect pre))))

(: untag/arr (Set-Tag -> expression))
(define (untag/arr tag)
  (define fwd (untag/fwd tag))
  (define comp (untag/comp tag))
  (expression (λ (r) (expression-meaning empty fwd comp))))
