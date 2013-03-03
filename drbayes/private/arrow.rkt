#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "omega.rkt"
         "rect.rkt"
         "indexes.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Expression type

;; An expression is a function from an index prefix to the expression's meaning
(struct: expression ([fun : (Omega-Idx -> expression-meaning)])
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

(define-type Forward-Fun (Omega Value Branches-Rect -> Value))

(: run-expression (case-> (expression -> expression-meaning)
                          (expression Omega-Idx -> expression-meaning)))
(define (run-expression e [r empty])
  ((expression-fun e) r))

;; A computation is a function from a domain and branches to its meaning
(define-type Computation
  ((U Empty-Set Omega-Rect) Rect Branches-Rect -> computation-meaning))

;; A computation means:
;;  1. A branches rectangle (which bounds the branches the forward computation can take)
;;  2. The approximate range of its forward function
;;  3. A function that computes approximate preimages under its forward function
(struct: computation-meaning ([branches : Branches-Rect]
                              [range : Rect]
                              [preimage : Preimage-Fun])
  #:transparent)

(define-type Preimage-Fun (Rect -> (Values (U Empty-Set Omega-Rect) Rect)))

;; ===================================================================================================
;; Conveniences

(define-type Simple-Preimage-Fun (Nonempty-Rect -> (Values (U Empty-Set Omega-Rect) Rect)))

(: simple-preimage ((U Empty-Set Omega-Rect) Rect Rect Simple-Preimage-Fun -> Preimage-Fun))
;; Wraps a Prim-Preimage-Fun with code that ensures the argument is a subset of the range and is
;; nonempty; also performs an optimization
(define ((simple-preimage Ω Γ range pre) B)
  (let ([B  (rect-intersect range B)])
    (cond [(empty-set? B)  (values empty-set empty-set)]
          [(eq? B range)   (values Ω Γ)]  ; optimization
          [else  (pre B)]
          #;
          [else
           (define-values (new-Ω new-Γ) (pre B))
           (values (omega-rect-intersect Ω new-Ω)
                   (rect-intersect Γ new-Γ))])))

(define-type Simple-Computation (Omega-Rect Nonempty-Rect Branches-Rect -> computation-meaning))

(define comp-hash-hits 0)
(define comp-hash-misses 0)
(define comp-hash-eq-misses 0)
(define (print-comp-hash-stats)
  (printf "comp-hash-hits = ~v~ncomp-hash-misses = ~v~ncomp-hash-eq-misses = ~v~n"
          comp-hash-hits
          comp-hash-misses
          comp-hash-eq-misses))

(: simple-computation (Simple-Computation -> Computation))
(define (simple-computation comp)
  (define: last-Ω : (U #f Omega-Rect)  #f)
  (define: last-Γ : (U #f Nonempty-Rect)  #f)
  (define: last-Z : (U #f Branches-Rect)  #f)
  (define: last-m : (U #f computation-meaning)  #f)
  (λ (Ω Γ Z)
    (cond [(or (empty-set? Ω) (empty-set? Γ))  (bottom/comp Ω Γ Z)]
          [else
           (let ([m  last-m])
             (cond [(and (eq? Ω last-Ω) (eq? Γ last-Γ) (eq? last-Z Z) m)
                    (set! comp-hash-hits (+ 1 comp-hash-hits))
                    m]
                   [else
                    #;
                    (begin
                      (when (and (not (eq? Ω last-Ω)) (equal? Ω last-Ω))
                        (printf "Ω = ~v~n" Ω)
                        (set! comp-hash-eq-misses (+ 1 comp-hash-eq-misses)))
                      (when (and (not (eq? Γ last-Γ)) (equal? Γ last-Γ))
                        (printf "Γ = ~v~n" Γ)
                        (set! comp-hash-eq-misses (+ 1 comp-hash-eq-misses)))
                      (when (and (not (eq? Z last-Z)) (equal? Z last-Z))
                        (printf "Z = ~v~n" Z)
                        (set! comp-hash-eq-misses (+ 1 comp-hash-eq-misses))))
                    (set! comp-hash-misses (+ 1 comp-hash-misses))
                    (define m (comp Ω Γ Z))
                    (set! last-Ω Ω)
                    (set! last-Γ Γ)
                    (set! last-Z Z)
                    (set! last-m m)
                    m]))]
          #;
          [else  (comp Ω Γ Z)])))

(: simple-computation/domain (Rect Simple-Computation -> Computation))
(define (simple-computation/domain domain comp)
  (cond [(empty-set? domain)  bottom/comp]
        [else
         (define: last-Γ : (U #f Nonempty-Rect)  #f)
         (define: last-m : (U #f computation-meaning)  #f)
         (λ (Ω Γ Z)
           (let ([Γ  (rect-intersect Γ domain)])
             (cond [(or (empty-set? Ω) (empty-set? Γ))  (bottom/comp Ω Γ Z)]
                   [else
                    (let ([m  last-m])
                      (cond [(and (eq? Γ last-Γ) m)
                             (set! comp-hash-hits (+ 1 comp-hash-hits))
                             m]
                            [else
                             #;
                             (when (and (not (eq? Γ last-Γ)) (equal? Γ last-Γ))
                               (printf "simple: Γ = ~v~n" Γ)
                               (set! comp-hash-eq-misses (+ 1 comp-hash-eq-misses)))
                             (set! comp-hash-misses (+ 1 comp-hash-misses))
                             (define m (comp Ω Γ Z))
                             (set! last-Γ Γ)
                             (set! last-m m)
                             m]))]
                   #;
                   [else  (comp Ω Γ Z)])))]))

;; ===================================================================================================
;; Basic primitives

;; Empty function (aka bottom; i.e. has empty range)

(: bottom/fwd Forward-Fun)
(define (bottom/fwd ω γ z)
  (error 'bottom/arr "empty range"))

(: bottom/pre Preimage-Fun)
(define (bottom/pre B) (values empty-set empty-set))

(: bottom/comp Computation)
(define (bottom/comp Ω Γ Z)
  (computation-meaning Z empty-set bottom/pre))

(define bottom/arr
  (expression (λ (r) (expression-meaning empty bottom/fwd bottom/comp))))

;; Identity function

(: id/fwd Forward-Fun)
(define (id/fwd ω γ z) γ)

(: id/pre ((U Empty-Set Omega-Rect) Rect -> Preimage-Fun))
(define ((id/pre Ω Γ) B) (values Ω (rect-intersect Γ B)))

(: id/comp (-> Computation))
(define (id/comp)
  (simple-computation (λ (Ω Γ Z) (computation-meaning Z Γ (id/pre Ω Γ)))))

(define id/arr
  (expression (λ (r) (expression-meaning empty id/fwd (id/comp)))))

;; Constant functions

(: c/fwd (Value -> Forward-Fun))
(define ((c/fwd x) ω γ z) x)

(: c/pre ((U Empty-Set Omega-Rect) Rect Nonempty-Rect -> Preimage-Fun))
(define ((c/pre Ω Γ X) B)
  (if (empty-set? (rect-intersect X B))
      (values empty-set empty-set)
      (values Ω Γ)))

(: c/comp (Nonempty-Rect -> Computation))
(define (c/comp X)
  (simple-computation (λ (Ω Γ Z) (computation-meaning Z X (c/pre Ω Γ X)))))

(: c/arr (Value -> expression))
(define (c/arr x)
  (define fwd (c/fwd x))
  (define X (value->singleton x))
  (expression (λ (r) (expression-meaning empty fwd (c/comp X)))))

;; ===================================================================================================
;; Arrow composition (reverse composition)

(: rcompose/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((rcompose/fwd f-fwd g-fwd) ω γ z)
  (let* ([γ  (f-fwd ω γ z)]
         [γ  (g-fwd ω γ z)])
    γ))

(: rcompose/pre (Preimage-Fun Preimage-Fun Omega-Rect -> Simple-Preimage-Fun))
(define ((rcompose/pre f-pre g-pre Ω) C)
  (define-values (Ωg B) (g-pre C))
  (define-values (Ωf Γ) (f-pre B))
  (values (unit-omega-rect-node/last Ω Ωf Ωg) Γ))

(: rcompose/comp (Computation Computation -> Computation))
(define (rcompose/comp f-comp g-comp)
  (simple-computation
   (λ (Ω Γf Z)
     (define Ωf (omega-rect-fst Ω))
     (define Ωg (omega-rect-snd Ω))
     (define Zf (branches-rect-fst Z))
     (define Zg (branches-rect-snd Z))
     (match-define (computation-meaning new-Zf Γg f-pre)    (f-comp Ωf Γf Zf))
     (match-define (computation-meaning new-Zg range g-pre) (g-comp Ωg Γg Zg))
     (define new-Z (branches-rect-node/last Z 'tf new-Zf new-Zg))
     (define pre (rcompose/pre f-pre g-pre Ω))
     (computation-meaning new-Z range (simple-preimage Ω Γf range pre)))))

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
(define ((pair/fwd fst-fwd snd-fwd) ω γ z)
  (cons (fst-fwd ω γ z) (snd-fwd ω γ z)))

(: pair/pre (Preimage-Fun Preimage-Fun Omega-Rect Nonempty-Rect Rect Rect -> Simple-Preimage-Fun))
(define (pair/pre fst-pre snd-pre Ω Γ fst-range snd-range)
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (B1×B2)
    (match-define (pair-rect B1 B2) B1×B2)
    (define fst? (not (eq? B1 fst-range)))
    (define snd? (not (eq? B2 snd-range)))
    (cond [(and fst? snd?)  (define-values (new-Ω1 A1) (fst-pre B1))
                            (define-values (new-Ω2 A2) (snd-pre B2))
                            (values (cond [(and (eq? new-Ω1 Ω1) (eq? new-Ω2 Ω2))  Ω]
                                          [else  (unit-omega-rect-node new-Ω1 new-Ω2)])
                                    (rect-intersect A1 A2))]
          [fst?  (define-values (new-Ω1 A1) (fst-pre B1))
                 (values (if (eq? new-Ω1 Ω1) Ω (unit-omega-rect-node new-Ω1 Ω2)) A1)]
          [snd?  (define-values (new-Ω2 A2) (snd-pre B2))
                 (values (if (eq? new-Ω2 Ω2) Ω (unit-omega-rect-node Ω1 new-Ω2)) A2)]
          [else  (values Ω Γ)])))

(: pair/comp (Computation Computation -> Computation))
(define (pair/comp fst-comp snd-comp)
  (simple-computation
   (λ (Ω Γ Z)
     (define Ω1 (omega-rect-fst Ω))
     (define Ω2 (omega-rect-snd Ω))
     (define Z1 (branches-rect-fst Z))
     (define Z2 (branches-rect-snd Z))
     (match-define (computation-meaning new-Z1 fst-range fst-pre) (fst-comp Ω1 Γ Z1))
     (match-define (computation-meaning new-Z2 snd-range snd-pre) (snd-comp Ω2 Γ Z2))
     (define new-Z (branches-rect-node/last Z 'tf new-Z1 new-Z2))
     (define range (pair-rect fst-range snd-range))
     (define pre (pair/pre fst-pre snd-pre Ω Γ fst-range snd-range))
     (computation-meaning new-Z range (simple-preimage Ω Γ range pre)))))

(: pair/arr (expression expression -> expression))
(define (pair/arr fst-expr snd-expr)
  (expression
   (λ (r)
     (match-define (expression-meaning fst-idxs fst-fwd fst-comp)
       (run-expression fst-expr (cons 0 r)))
     (match-define (expression-meaning snd-idxs snd-fwd snd-comp)
       (run-expression snd-expr (cons 1 r)))
     (expression-meaning (append fst-idxs snd-idxs)
                         (pair/fwd fst-fwd snd-fwd)
                         (pair/comp fst-comp snd-comp)))))

;; ===================================================================================================
;; Conditionals

(define-predicate if-bad-branch? 'if-bad-branch)

(: if/fwd (Omega-Idx Forward-Fun (Promise Forward-Fun) (Promise Forward-Fun) -> Forward-Fun))
(define ((if/fwd idx c-fwd t-fwd f-fwd) ω γ z)
  (define c (c-fwd ω γ z))
  (define b (branches-rect-ref z idx))
  (cond [(or (not (boolean? c)) (not (boolean-set-member? b c)))  (raise 'if-bad-branch)]
        [c     ((force t-fwd) ω γ z)]
        [else  ((force f-fwd) ω γ z)]))

(: if/comp (Boolean Computation (Promise expression-meaning) (Promise expression-meaning)
                    -> Computation))
(define (if/comp strict? c-comp t-meaning f-meaning)
  (simple-computation
   (λ (orig-Ω orig-Γ orig-Z)
     (define Ωc (omega-rect-fst orig-Ω))
     (define Ωtf (omega-rect-snd orig-Ω))
     (define Ωt (omega-rect-fst Ωtf))
     (define Ωf (omega-rect-snd Ωtf))
     
     (: make-omega-rect ((U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect)
                                                  -> (U Empty-Set Omega-Rect)))
     (define (make-omega-rect new-Ωc new-Ωt new-Ωf)
       (cond [(and (eq? Ωt new-Ωt) (eq? Ωf new-Ωf))
              (cond [(eq? Ωc new-Ωc)  orig-Ω]
                    [else  (unit-omega-rect-node new-Ωc Ωtf)])]
             [else
              (unit-omega-rect-node new-Ωc (unit-omega-rect-node new-Ωt new-Ωf))]))
     
     (define b (branches-rect-value orig-Z))
     (define Zc (branches-rect-fst orig-Z))
     (define Ztf (branches-rect-snd orig-Z))
     (define Zt (branches-rect-fst Ztf))
     (define Zf (branches-rect-snd Ztf))
     
     (: make-branches-rect (Boolean-Set Branches-Rect Branches-Rect Branches-Rect -> Branches-Rect))
     (define (make-branches-rect new-b new-Zc new-Zt new-Zf)
       (cond [(and (eq? Zt new-Zt) (eq? Zf new-Zf))
              (cond [(and (eq? Zc new-Zc) (eq? b new-b))  orig-Z]
                    [else  (branches-rect-node new-b new-Zc Ztf)])]
             [else
              (branches-rect-node new-b new-Zc (branches-rect-node 'tf new-Zt new-Zf))]))
     
     (match-let ([(computation-meaning Zc c-range c-pre)  (c-comp Ωc orig-Γ Zc)])
       (define-values (Ωct Γt)
         (cond [(rect-member? c-range #t)  (c-pre 't)]
               [else  (values empty-set empty-set)]))
       
       (define-values (Ωcf Γf)
         (cond [(rect-member? c-range #f)  (c-pre 'f)]
               [else  (values empty-set empty-set)]))
       
       (define-values (bt? bf?) (boolean-set->booleans b))
       
       ;; t? = #t if it's possible to take the true branch
       ;; f? = #t if it's possible to take the false branch
       (define-values (t? f?)
         (values (and bt? (not (empty-set? Ωct)) (not (empty-set? Γt)))
                 (and bf? (not (empty-set? Ωcf)) (not (empty-set? Γf)))))
       
       (: t/pre (Preimage-Fun -> Preimage-Fun))
       (define ((t/pre t-pre) B)
         (let-values ([(Ωt At)  (t-pre B)])
           (values (make-omega-rect Ωct Ωt Ωf) At)))
       
       (: f/pre (Preimage-Fun -> Preimage-Fun))
       (define ((f/pre f-pre) B)
         (let-values ([(Ωf Af)  (f-pre B)])
           (values (make-omega-rect Ωcf Ωt Ωf) Af)))
       
       (cond
         [(and t? f?)
          (cond
            [strict?
             (define t-comp (expression-meaning-computation (force t-meaning)))
             (define f-comp (expression-meaning-computation (force f-meaning)))
             (match-let* ([(computation-meaning Zt t-range t-pre)  (t-comp Ωt Γt Zt)]
                          [(computation-meaning Zf f-range f-pre)  (f-comp Ωf Γf Zf)])
               (define range (rect-join t-range f-range))
               
               (: if/pre Simple-Preimage-Fun)
               (define (if/pre B)
                 (let-values ([(Ω1 A1)  ((t/pre t-pre) (rect-intersect t-range B))]
                              [(Ω2 A2)  ((f/pre f-pre) (rect-intersect f-range B))])
                   (values (omega-rect-join Ω1 Ω2) (rect-join A1 A2))))
               
               (let-values ([(Ωt Γt)  ((t/pre t-pre) t-range)]
                            [(Ωf Γf)  ((f/pre f-pre) f-range)])
                 (define Ω (omega-rect-join Ωt Ωf))
                 (define Γ (rect-join Γt Γf))
                 (define Z (make-branches-rect 'tf Zc Zt Zf))
                 (computation-meaning Z range (simple-preimage Ω Γ range if/pre))))]
            [else
             (define Ω (make-omega-rect (omega-rect-join Ωct Ωcf) Ωt Ωf))
             (define Γ (rect-join Γt Γf))
             (define Z (make-branches-rect 'tf Zc branches-rect branches-rect))
             (define range universal-set)
             (computation-meaning Z range (simple-preimage Ω Γ range (λ (B) (values Ω Γ))))])]
         [t?
          (define t-comp (expression-meaning-computation (force t-meaning)))
          (match-let ([(computation-meaning Zt t-range t-pre)  (t-comp Ωt Γt Zt)])
            (define Ω (make-omega-rect Ωct Ωt Ωf))
            (define Z (make-branches-rect 't Zc Zt Zf))
            (computation-meaning Z t-range (simple-preimage Ω Γt t-range (t/pre t-pre))))]
         [f?
          (define f-comp (expression-meaning-computation (force f-meaning)))
          (match-let ([(computation-meaning Zf f-range f-pre)  (f-comp Ωf Γf Zf)])
            (define Ω (make-omega-rect Ωcf Ωt Ωf))
            (define Z (make-branches-rect 'f Zc Zt Zf))
            (computation-meaning Z f-range (simple-preimage Ω Γf f-range (f/pre f-pre))))]
         [else
          (bottom/comp empty-set empty-set orig-Z)])))))

(: if/arr (Boolean -> (expression expression expression -> expression)))
(define ((if/arr strict?) c-expr t-expr f-expr)
  (expression
   (λ (r)
     (define idx (reverse r))
     (match-define (expression-meaning c-idxs c-fwd c-comp) (run-expression c-expr (cons 0 r)))
     (define t-meaning (delay (run-expression t-expr (list* 0 1 r))))
     (define f-meaning (delay (run-expression f-expr (list* 1 1 r))))
     (define t-idxs (delay (expression-meaning-indexes (force t-meaning))))
     (define f-idxs (delay (expression-meaning-indexes (force f-meaning))))
     (expression-meaning
      (append c-idxs (list (if-indexes idx t-idxs f-idxs)))
      (if/fwd idx c-fwd
              (delay (expression-meaning-forward (force t-meaning)))
              (delay (expression-meaning-forward (force f-meaning))))
      (if/comp strict? c-comp t-meaning f-meaning)))))

(define lazy-if/arr (if/arr #f))
(define strict-if/arr (if/arr #t))

;; ===================================================================================================
;; Random

(: random/fwd (Omega-Idx -> Forward-Fun))
(define ((random/fwd idx) ω γ z)
  (omega-ref ω idx))

(: random/pre (Omega-Rect Nonempty-Rect -> Simple-Preimage-Fun))
(define (random/pre Ω Γ)
  (define A (omega-rect-value Ω))
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (B)
    (cond [(eq? A B)  (values Ω Γ)]
          [(interval? B)  (values (omega-rect-node B Ω1 Ω2) Γ)]
          [else  (values empty-set empty-set)])))

(: random/comp (-> Computation))
(define (random/comp)
  (simple-computation
   (λ (Ω Γ Z)
     (define range (omega-rect-value Ω))
     (computation-meaning Z range (simple-preimage Ω Γ range (random/pre Ω Γ))))))

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

(: boolean/fwd (Omega-Idx Flonum -> Forward-Fun))
(define ((boolean/fwd r p) ω γ z)
  ((omega-ref ω r) . < . p))

(: boolean/pre
   (Omega-Rect Nonempty-Rect (U Empty-Set Interval) (U Empty-Set Interval) -> Simple-Preimage-Fun))
(define (boolean/pre Ω Γ It If)
  (define A (omega-rect-value Ω))
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (B)
    (define I (case B
                [(tf)  (rect-join It If)]
                [(t)   It]
                [(f)   If]
                [else  empty-set]))
    (cond [(eq? A I)  (values Ω Γ)]
          [(interval? I)  (values (omega-rect-node I Ω1 Ω2) Γ)]
          [else  (values empty-set empty-set)])))

(: boolean/comp (Interval Interval -> Computation))
(define (boolean/comp It If)
  (simple-computation
   (λ (Ω Γ Z)
     (define I (omega-rect-value Ω))
     (let ([It  (interval-intersect I It)]
           [If  (interval-intersect I If)])
       (define range (booleans->boolean-set (not (empty-set? It)) (not (empty-set? If))))
       (define pre (boolean/pre Ω Γ It If))
       (computation-meaning Z range (simple-preimage Ω Γ range pre))))))

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
;; Ref

(: ref/fwd (Idx -> Forward-Fun))
(define ((ref/fwd j) ω γ z)
  (value-ref γ j))

(: ref/pre (Omega-Rect Nonempty-Rect Idx -> Simple-Preimage-Fun))
(define ((ref/pre Ω Γ j) B)
  (values Ω (rect-set Γ j B)))

(: ref/comp (Idx -> Computation))
(define (ref/comp j)
  (simple-computation
   (λ (Ω Γ Z)
     (define range (rect-ref Γ j))
     (computation-meaning Z range (simple-preimage Ω Γ range (ref/pre Ω Γ j))))))

(: ref/arr (Idx -> expression))
(define (ref/arr j)
  (define fwd (ref/fwd j))
  (expression
   (λ (r)
     (expression-meaning empty fwd (ref/comp j)))))

;; ===================================================================================================
;; Monotone R -> R functions

(: monotone/fwd (Symbol (Flonum -> Flonum) -> Forward-Fun))
(define ((monotone/fwd name f) ω γ z)
  (if (flonum? γ) (f γ) (raise-argument-error name "Flonum" γ)))

(: monotone-range (Nonempty-Rect Interval (Flonum -> Flonum) Boolean -> Rect))
(define (monotone-range domain f-range f fx?)
  (match-define (interval a b a? b?) domain)
  (cond [fx?   (rect-intersect f-range (interval (f a) (f b) a? b?))]
        [else  (rect-intersect f-range (interval (f b) (f a) b? a?))]))

(: monotone/pre (Omega-Rect Nonempty-Rect (Flonum -> Flonum) Boolean -> Simple-Preimage-Fun))
(define ((monotone/pre Ω Γ g fx?) B)
  (match B
    [(interval a b a? b?)
     (values Ω (cond [fx?   (rect-intersect Γ (interval (g a) (g b) a? b?))]
                     [else  (rect-intersect Γ (interval (g b) (g a) b? a?))]))]
    [_  (values empty-set empty-set)]))

(: monotone/comp (Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean -> Computation))
(define (monotone/comp f-domain f-range f g fx?)
  (simple-computation/domain
   f-domain
   (λ (Ω Γ Z)
     (define range (monotone-range Γ f-range f fx?))
     (computation-meaning Z range (simple-preimage Ω Γ range (monotone/pre Ω Γ g fx?))))))

(: monotone/arr (Symbol Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                        -> expression))
(define (monotone/arr name f-domain f-range f g fx?)
  (expression
   (λ (r)
     (expression-meaning empty
                         (monotone/fwd name f)
                         (monotone/comp f-domain f-range f g fx?)))))

;; ===================================================================================================
;; Monotone R x R -> R functions

(: monotone2d/fwd (Symbol (Flonum Flonum -> Flonum) -> Forward-Fun))
(define ((monotone2d/fwd name f) ω γ z)
  (match γ
    [(cons (? flonum? x) (? flonum? y))  (f x y)]
    [_  (raise-argument-error name "(Pair Flonum Flonum)" γ)]))

(: monotone2d-range (Nonempty-Rect Interval (Flonum Flonum -> Flonum) Boolean Boolean -> Rect))
(define (monotone2d-range domain f-range f fx? fy?)
  (match-define (pair-rect (interval xa xb xa? xb?) (interval ya yb ya? yb?)) domain)
  (let-values ([(xa xb xa? xb?)  (if fx? (values xa xb xa? xb?) (values xb xa xb? xa?))]
               [(ya yb ya? yb?)  (if fy? (values ya yb ya? yb?) (values yb ya yb? ya?))])
    (rect-intersect f-range (interval (f xa ya) (f xb yb) (and xa? ya?) (and xb? yb?)))))

(: monotone2d/pre (Omega-Rect Nonempty-Rect
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              -> Simple-Preimage-Fun))
(define (monotone2d/pre Ω Γ g gz? gy? h hz? hx?)
  (match-define (pair-rect (interval xa xb xa? xb?) (interval ya yb ya? yb?)) Γ)
  (λ (B)
    (match B
      [(interval za zb za? zb?)
       (define X
         (let-values ([(za zb za? zb?)  (if gz? (values za zb za? zb?) (values zb za zb? za?))]
                      [(ya yb ya? yb?)  (if gy? (values ya yb ya? yb?) (values yb ya yb? ya?))])
           (interval (g za ya) (g zb yb) (and za? ya?) (and zb? yb?))))
       (define Y
         (let-values ([(za zb za? zb?)  (if hz? (values za zb za? zb?) (values zb za zb? za?))]
                      [(xa xb xa? xb?)  (if hx? (values xa xb xa? xb?) (values xb xa xb? xa?))])
           (interval (h za xa) (h zb xb) (and za? xa?) (and zb? xb?))))
       (values Ω (rect-intersect Γ (pair-rect X Y)))]
      [_
       (values empty-set empty-set)])))

(: monotone2d/comp (Pair-Rect Interval
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              -> Computation))
(define (monotone2d/comp f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (simple-computation/domain
   f-domain
   (λ (Ω Γ Z)
     (define range (monotone2d-range Γ f-range f fx? fy?))
     (define pre (simple-preimage Ω Γ range (monotone2d/pre Ω Γ g gz? gy? h hz? hx?)))
     (computation-meaning Z range pre))))

(: monotone2d/arr (Symbol Pair-Rect Interval
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    -> expression))
(define (monotone2d/arr name f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (expression
   (λ (r)
     (expression-meaning empty
                         (monotone2d/fwd name f)
                         (monotone2d/comp f-domain f-range f fx? fy? g gz? gy? h hz? hx?)))))

;; ===================================================================================================
;; Predicates

(: predicate/fwd ((Value -> Boolean) -> Forward-Fun))
(define ((predicate/fwd pred?) ω γ z)
  (pred? γ))

(: predicate-range (Rect Rect -> (U Empty-Set Boolean-Set)))
(define (predicate-range true-set false-set)
  (booleans->boolean-set (not (empty-set? true-set))
                         (not (empty-set? false-set))))

(: predicate/pre (Omega-Rect Rect Rect -> Simple-Preimage-Fun))
(define ((predicate/pre Ω true-set false-set) B)
  (values Ω (case B
              [(tf)  (rect-join true-set false-set)]
              [(t)   true-set]
              [(f)   false-set]
              [else  empty-set])))

(: predicate/comp (Rect Rect -> Computation))
(define (predicate/comp true-set false-set)
  (simple-computation/domain
   (rect-join true-set false-set)
   (λ (Ω Γ Z)
     (let ([true-set   (rect-intersect Γ true-set)]
           [false-set  (rect-intersect Γ false-set)])
       (define range (predicate-range true-set false-set))
       (define pre (simple-preimage Ω Γ range (predicate/pre Ω true-set false-set)))
       (computation-meaning Z range pre)))))

(: predicate/arr ((Value -> Boolean) Nonempty-Rect Nonempty-Rect -> expression))
(define (predicate/arr pred? true-set false-set)
  (define fwd (predicate/fwd pred?))
  (expression
   (λ (r)
     (expression-meaning empty fwd (predicate/comp true-set false-set)))))
