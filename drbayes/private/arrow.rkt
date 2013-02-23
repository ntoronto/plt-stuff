#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/function
         racket/promise
         racket/set
         math/base
         math/flonum
         math/distributions
         "rect.rkt"
         "types.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Interval splitters

(define-type Interval-Splitter
  (Interval Flonum -> (Values (Listof Interval) (Listof Positive-Flonum))))

(define-type Indexes (Listof (U Omega-Idx (Pair Omega-Idx Interval-Splitter) if-indexes)))
(struct: if-indexes ([index : Omega-Idx] [true : (Promise Indexes)] [false : (Promise Indexes)])
  #:transparent)

(: interval-split Interval-Splitter)
(define (interval-split I min-ivl)
  (match-define (interval a b a? b?) I)
  (cond [((fl- b a) . fl<= . min-ivl)  (values (list I) (list 1.0))]
        [else
         (define c (* 0.5 (+ a b)))
         (define m1 (fl- c a))
         (define m2 (fl- b c))
         (cond [(and (positive? m1) (positive? m2))
                (values (list (Interval a c a? #t) (Interval c b #f b?)) (list m1 m2))]
               [else
                (values (list I) (list 1.0))])]))

(: intersect-and-filter ((Listof Interval) Interval -> (Values (Listof Interval)
                                                               (Listof Positive-Flonum))))
(define (intersect-and-filter Is A)
  (let: loop ([Is Is] [new-Is : (Listof Interval)  empty] [ps : (Listof Positive-Flonum)  empty])
    (cond [(empty? Is)  (values (reverse new-Is) (reverse ps))]
          [else
           (define I (interval-intersect (first Is) A))
           (cond [(empty-set? I)  (loop (rest Is) new-Is ps)]
                 [else
                  (define p (interval-measure I))
                  (cond [(p . <= . 0.0)  (loop (rest Is) new-Is ps)]
                        [else  (loop (rest Is) (cons I new-Is) (cons p ps))])])])))

(: make-constant-splitter ((Listof Interval) -> Interval-Splitter))
(define (make-constant-splitter Is)
  (when (not (apply rect-disjoint? Is))
    (raise-argument-error 'make-binary-split "disjoint (Listof Interval)" Is))
  (let-values ([(Is _)  (intersect-and-filter Is unit-interval)])
    (λ (A _) (intersect-and-filter Is A))))

;; ===================================================================================================
;; Expression type

;; An expression is a function from index bounds to its meaning
(struct: expression ([fun : (Omega-Idx Omega-Idx -> expression-meaning)])
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
                          (expression Omega-Idx Omega-Idx -> expression-meaning)))
(define (run-expression e [r0 0] [r1 1])
  ((expression-fun e) r0 r1))

;; A computation is a function from a domain and branches to its meaning
(define-type Computation
  ((U Empty-Set Omega-Rect) Rect (U Empty-Set Branches-Rect) -> computation-meaning))

;; A computation means:
;;  1. A branches rectangle (which bounds the branches the forward computation can take)
;;  2. The approximate range of its forward function
;;  3. A function that computes approximate preimages under its forward function
(struct: computation-meaning ([branches : (U Empty-Set Branches-Rect)]
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
  (let ([B  (rect-intersect B range)])
    (cond [(empty-set? B)  (values empty-set empty-set)]
          [(eq? B range)   (values Ω Γ)]  ; optimization
          [else  (pre B)])))

(define-type Simple-Computation (Omega-Rect Nonempty-Rect Branches-Rect -> computation-meaning))

(struct: comp-rec ([omega : (U #f Omega-Rect)]
                   [gamma : (U #f Nonempty-Rect)]
                   [branches : (U #f Branches-Rect)]
                   [meaning : (U #f computation-meaning)])
  #:transparent
  #:mutable)

(: comp-hash (HashTable Omega-Idx comp-rec))
(define comp-hash (make-hash))

(define comp-hash-hits 0)
(define comp-hash-misses 0)
(define (print-comp-hash-stats)
  (printf "hits = ~v~nmisses = ~v~n" comp-hash-hits comp-hash-misses))

(: simple-computation (Omega-Idx Simple-Computation -> Computation))
(define (simple-computation r comp)
  (λ (Ω Γ bs)
    (cond [(or (empty-set? Ω) (empty-set? Γ) (empty-set? bs))  (bottom/comp Ω Γ bs)]
          [else  (comp Ω Γ bs)])))

(: simple-computation/domain (Omega-Idx Rect Simple-Computation -> Computation))
(define (simple-computation/domain r domain comp)
  (cond [(empty-set? domain)  bottom/comp]
        [else
         (λ (Ω Γ bs)
           (let ([Γ  (rect-intersect Γ domain)])
             (cond [(or (empty-set? Ω) (empty-set? Γ) (empty-set? bs))  (bottom/comp Ω Γ bs)]
                   [else  (comp Ω Γ bs)])))]))

;; ===================================================================================================
;; Basic primitives

;; Empty function (aka bottom; i.e. has empty range)

(: bottom/fwd Forward-Fun)
(define (bottom/fwd ω γ bs)
  (error 'bottom/arr "empty range"))

(: bottom/pre Preimage-Fun)
(define (bottom/pre B) (values empty-set empty-set))

(: bottom/comp Computation)
(define (bottom/comp Ω Γ bs)
  (computation-meaning bs empty-set bottom/pre))

(define bottom/arr
  (expression
   (λ (r0 r1)
     (expression-meaning empty bottom/fwd bottom/comp))))

;; Identity function

(: id/fwd Forward-Fun)
(define (id/fwd ω γ bs) γ)

(: id/pre ((U Empty-Set Omega-Rect) Rect -> Preimage-Fun))
(define ((id/pre Ω Γ) B) (values Ω (rect-intersect B Γ)))

(: id/comp (Omega-Idx -> Computation))
(define (id/comp r)
  (simple-computation
   r (λ (Ω Γ bs)
       (computation-meaning bs Γ (id/pre Ω Γ)))))

(define id/arr
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty id/fwd (id/comp r)))))

;; Constant functions

(: c/fwd (Value -> Forward-Fun))
(define ((c/fwd x) ω γ bs) x)

(: c/pre ((U Empty-Set Omega-Rect) Rect Nonempty-Rect -> Preimage-Fun))
(define ((c/pre Ω Γ X) B)
  (if (empty-set? (rect-intersect X B))
      (values empty-set empty-set)
      (values Ω Γ)))

(: c/comp (Omega-Idx Value -> Computation))
(define (c/comp r x)
  (define X (value->singleton x))
  (simple-computation
   r (λ (Ω Γ bs)
       (computation-meaning bs X (c/pre Ω Γ X)))))

(: c/arr (Value -> expression))
(define (c/arr x)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty (c/fwd x) (c/comp r x)))))

;; ---------------------------------------------------------------------------------------------------
;; Application

(: ap/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((ap/fwd f-fwd g-fwd) ω γ bs)
  (let* ([γ  (g-fwd ω γ bs)]
         [γ  (f-fwd ω γ bs)])
    γ))

(: ap/pre (Preimage-Fun Preimage-Fun -> Simple-Preimage-Fun))
(define ((ap/pre f-pre g-pre) C)
  (let*-values ([(Ωf B)  (f-pre C)]
                [(Ωg A)  (g-pre B)])
    (values (omega-rect-intersect Ωf Ωg) A)))

(: ap/comp (Omega-Idx Computation Computation -> Computation))
(define (ap/comp r f-comp g-comp)
  (simple-computation
   r (λ (Ω Γ bs)
       (match-let* ([(computation-meaning bs g-range g-pre)  (g-comp Ω Γ bs)]
                    [(computation-meaning bs f-range f-pre)  (f-comp Ω g-range bs)])
         (computation-meaning bs f-range (simple-preimage Ω Γ f-range (ap/pre f-pre g-pre)))))))

(: ap/arr (expression expression -> expression))
(define (ap/arr f-expr g-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (match-define (expression-meaning g-idxs g-fwd g-comp) (run-expression g-expr r r1))
     (match-define (expression-meaning f-idxs f-fwd f-comp) (run-expression f-expr r0 r))
     (expression-meaning (append f-idxs g-idxs)
                         (ap/fwd f-fwd g-fwd)
                         (ap/comp r f-comp g-comp)))))

;; ---------------------------------------------------------------------------------------------------
;; Reverse application

(: rap/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((rap/fwd f-fwd g-fwd) ω γ bs)
  (let* ([γ  (f-fwd ω γ bs)]
         [γ  (g-fwd ω γ bs)])
    γ))

(: rap/pre (Preimage-Fun Preimage-Fun -> Simple-Preimage-Fun))
(define ((rap/pre f-pre g-pre) C)
  (let*-values ([(Ωg B)  (g-pre C)]
                [(Ωf A)  (f-pre B)])
    (values (omega-rect-intersect Ωf Ωg) A)))

(: rap/comp (Omega-Idx Computation Computation -> Computation))
(define (rap/comp r f-comp g-comp)
  (simple-computation
   r (λ (Ω Γ bs)
       (match-let* ([(computation-meaning bs f-range f-pre)  (f-comp Ω Γ bs)]
                    [(computation-meaning bs g-range g-pre)  (g-comp Ω f-range bs)])
         (computation-meaning bs g-range (simple-preimage Ω Γ g-range (rap/pre f-pre g-pre)))))))

(: rap/arr (expression expression -> expression))
(define (rap/arr f-expr g-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (match-define (expression-meaning f-idxs f-fwd f-comp) (run-expression f-expr r0 r))
     (match-define (expression-meaning g-idxs g-fwd g-comp) (run-expression g-expr r r1))
     (expression-meaning (append f-idxs g-idxs)
                         (rap/fwd f-fwd g-fwd)
                         (rap/comp r f-comp g-comp)))))

;; ---------------------------------------------------------------------------------------------------
;; Pairs and lists

(define null/arr (c/arr null))

(: list/arr (expression * -> expression))
(define (list/arr . es)
  (foldr pair/arr null/arr es))

(: pair/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((pair/fwd fst-fwd snd-fwd) ω γ bs)
  (cons (fst-fwd ω γ bs) (snd-fwd ω γ bs)))

(: pair/pre (Preimage-Fun Preimage-Fun Omega-Rect Nonempty-Rect Rect Rect -> Simple-Preimage-Fun))
(define ((pair/pre fst-pre snd-pre Ω Γ fst-range snd-range) B1×B2)
  (match-define (pair-rect B1 B2) B1×B2)
  (define fst? (not (eq? B1 fst-range)))
  (define snd? (not (eq? B2 snd-range)))
  (cond [(and fst? snd?)
         (let-values ([(Ω1 A1)  (fst-pre B1)]
                      [(Ω2 A2)  (snd-pre B2)])
           (values (omega-rect-intersect Ω1 Ω2)
                   (rect-intersect A1 A2)))]
        [fst?  (fst-pre B1)]
        [snd?  (snd-pre B2)]
        [else  (values Ω Γ)]))

(: pair/comp (Omega-Idx Computation Computation -> Computation))
(define (pair/comp r fst-comp snd-comp)
  (simple-computation
   r (λ (Ω Γ bs)
       (match-let* ([(computation-meaning bs fst-range fst-pre)  (fst-comp Ω Γ bs)]
                    [(computation-meaning bs snd-range snd-pre)  (snd-comp Ω Γ bs)])
         (define range (pair-rect fst-range snd-range))
         (define pre (pair/pre fst-pre snd-pre Ω Γ fst-range snd-range))
         (computation-meaning bs range (simple-preimage Ω Γ range pre))))))

(: pair/arr (expression expression -> expression))
(define (pair/arr fst-expr snd-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (match-define (expression-meaning fst-idxs fst-fwd fst-comp) (run-expression fst-expr r0 r))
     (match-define (expression-meaning snd-idxs snd-fwd snd-comp) (run-expression snd-expr r r1))
     (expression-meaning (append fst-idxs snd-idxs)
                         (pair/fwd fst-fwd snd-fwd)
                         (pair/comp r fst-comp snd-comp)))))

;; ---------------------------------------------------------------------------------------------------
;; Random

(: random/fwd (Omega-Idx -> Forward-Fun))
(define ((random/fwd r) ω γ bs)
  (omega-ref ω r))

(: random/pre (Omega-Rect Nonempty-Rect Omega-Idx -> Simple-Preimage-Fun))
(define ((random/pre Ω Γ r) B)
  (cond [(interval? B)  (values (omega-rect-set Ω r B) Γ)]
        [else  (values empty-set empty-set)]))

(: random/comp (Omega-Idx -> Computation))
(define (random/comp r)
  (simple-computation
   r (λ (Ω Γ bs)
       (define range (omega-rect-ref Ω r))
       (computation-meaning bs range (simple-preimage Ω Γ range (random/pre Ω Γ r))))))

(: random/arr expression)
(define random/arr
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning (list r) (random/fwd r) (random/comp r)))))

;; ---------------------------------------------------------------------------------------------------
;; Random boolean

(: random-boolean/fwd (Omega-Idx Flonum -> Forward-Fun))
(define ((random-boolean/fwd r p) ω γ bs)
  ((omega-ref ω r) . < . p))

(: random-boolean/pre
   (Omega-Rect Nonempty-Rect Omega-Idx (U Empty-Set Interval) (U Empty-Set Interval)
               -> Simple-Preimage-Fun))
(define ((random-boolean/pre Ω Γ r It If) B)
  (define I (case B
              [(tf)  (rect-join It If)]
              [(t)   It]
              [(f)   If]
              [else  empty-set]))
  (cond [(interval? I)  (values (omega-rect-set Ω r I) Γ)]
        [else  (values empty-set empty-set)]))

(: random-boolean/comp (Omega-Idx Interval Interval -> Computation))
(define (random-boolean/comp r It If)
  (simple-computation
   r (λ (Ω Γ bs)
       (define I (omega-rect-ref Ω r))
       (let ([It  (interval-intersect I It)]
             [If  (interval-intersect I If)])
         (define range (booleans->boolean-set (not (empty-set? It)) (not (empty-set? If))))
         (define pre (random-boolean/pre Ω Γ r It If))
         (computation-meaning bs range (simple-preimage Ω Γ range pre))))))

(: random-boolean/arr (Flonum -> expression))
(define (random-boolean/arr p)
  (cond
    [(and (p . > . 0.0) (p . < . 1.0))
     (define It (Interval 0.0 p #t #f))
     (define If (Interval p 1.0 #t #t))
     (define split (make-constant-splitter (list It If)))
     (expression
      (λ (r0 r1)
        (define r (omega-expr-idx r0 r1))
        (expression-meaning (list (cons r split))
                            (random-boolean/fwd r p)
                            (random-boolean/comp r It If))))]
    [(= p 0.0)  (c/arr #f)]
    [(= p 1.0)  (c/arr #t)]
    [else  (raise-argument-error 'boolean "probability" p)]))

;; ===================================================================================================
;; Primitives

;; ---------------------------------------------------------------------------------------------------
;; Ref

(: ref/fwd (Idx -> Forward-Fun))
(define ((ref/fwd j) ω γ bs)
  (value-ref γ j))

(: ref/pre (Omega-Rect Nonempty-Rect Idx -> Simple-Preimage-Fun))
(define ((ref/pre Ω Γ j) B)
  (values Ω (rect-set Γ j B)))

(: ref/comp (Omega-Idx Idx -> Computation))
(define (ref/comp r j)
  (simple-computation
   r (λ (Ω Γ bs)
       (define range (rect-ref Γ j))
       (computation-meaning bs range (simple-preimage Ω Γ range (ref/pre Ω Γ j))))))

(: ref/arr (Idx -> expression))
(define (ref/arr j)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty (ref/fwd j) (ref/comp r j)))))

;; ---------------------------------------------------------------------------------------------------
;; Monotone R -> R functions

(: monotone/fwd (Symbol (Flonum -> Flonum) -> Forward-Fun))
(define ((monotone/fwd name f) ω γ bs)
  (if (flonum? γ) (f γ) (raise-argument-error name "Flonum" γ)))

(: monotone-range (Nonempty-Rect Interval (Flonum -> Flonum) Boolean -> Rect))
(define (monotone-range domain f-range f fx?)
  (match-define (interval a b a? b?) domain)
  (cond [fx?   (rect-intersect (interval (f a) (f b) a? b?) f-range)]
        [else  (rect-intersect (interval (f b) (f a) b? a?) f-range)]))

(: monotone/pre (Omega-Rect Nonempty-Rect (Flonum -> Flonum) Boolean -> Simple-Preimage-Fun))
(define ((monotone/pre Ω Γ g fx?) B)
  (match B
    [(interval a b a? b?)
     (values Ω (cond [fx?   (rect-intersect (interval (g a) (g b) a? b?) Γ)]
                     [else  (rect-intersect (interval (g b) (g a) b? a?) Γ)]))]
    [_  (values empty-set empty-set)]))

(: monotone/comp (Omega-Idx Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                            -> Computation))
(define (monotone/comp r f-domain f-range f g fx?)
  (simple-computation/domain
   r f-domain
   (λ (Ω Γ bs)
     (define range (monotone-range Γ f-range f fx?))
     (computation-meaning bs range (simple-preimage Ω Γ range (monotone/pre Ω Γ g fx?))))))

(: monotone/arr (Symbol Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                        -> expression))
(define (monotone/arr name f-domain f-range f g fx?)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty
                         (monotone/fwd name f)
                         (monotone/comp r f-domain f-range f g fx?)))))

;; ---------------------------------------------------------------------------------------------------
;; Concrete monotone R -> R functions

(: scale/arr (Flonum -> expression))
(define (scale/arr y)
  (cond [(fl= y 0.0)  (c/arr 0.0)]
        [else  (monotone/arr 'scale/arr reals reals
                                      (λ: ([x : Flonum]) (fl* x y))
                                      (λ: ([z : Flonum]) (fl/ z y))
                                      (y . fl> . 0.0))]))

(: translate/arr (Flonum -> expression))
(define (translate/arr y)
  (monotone/arr 'translate/arr reals reals
                         (λ: ([x : Flonum]) (fl+ x y))
                         (λ: ([z : Flonum]) (fl- z y))
                         #t))

(: flneg (Flonum -> Flonum))
(define (flneg x) (fl* -1.0 x))

(: flsqr (Flonum -> Flonum))
(define (flsqr x) (fl* x x))

(: flinv (Flonum -> Flonum))
(define (flinv x) (fl/ 1.0 x))

(define neg/arr (monotone/arr 'neg/arr reals reals flneg flneg #f))
(define exp/arr (monotone/arr 'exp/arr reals nonnegative-reals flexp fllog #t))
(define log/arr (monotone/arr 'log/arr nonnegative-reals reals fllog flexp #t))
(define sqrt/arr (monotone/arr 'sqrt/arr nonnegative-reals nonnegative-reals flsqrt flsqr #t))
(define pos-inv/arr (monotone/arr 'inv/arr positive-reals positive-reals flinv flinv #t))
(define neg-inv/arr (monotone/arr 'inv/arr negative-reals negative-reals flinv flinv #f))

;; ---------------------------------------------------------------------------------------------------
;; Distributions from inverse cdf

(: inverse-cdf/arr (Symbol Interval (Flonum -> Flonum) (Flonum -> Flonum) -> expression))
(define (inverse-cdf/arr name range inv-cdf cdf)
  (monotone/arr name unit-interval range inv-cdf cdf #t))

(: cauchy-inv-cdf (Flonum -> Flonum))
(define (cauchy-inv-cdf p)
  (flcauchy-inv-cdf 0.0 1.0 p #f #f))

(: cauchy-cdf (Flonum -> Flonum))
(define (cauchy-cdf x)
  (flcauchy-cdf 0.0 1.0 x #f #f))

(: normal-inv-cdf (Flonum -> Flonum))
(define (normal-inv-cdf p)
  (flnormal-inv-cdf 0.0 1.0 p #f #f))

(: normal-cdf (Flonum -> Flonum))
(define (normal-cdf x)
  (flnormal-cdf 0.0 1.0 x #f #f))

(define cauchy/arr (inverse-cdf/arr 'cauchy/arr reals cauchy-inv-cdf cauchy-cdf))
(define normal/arr (inverse-cdf/arr 'normal/arr reals normal-inv-cdf normal-cdf))

;; ---------------------------------------------------------------------------------------------------
;; Monotone R R -> R functions

(: monotone2d/fwd (Symbol (Flonum Flonum -> Flonum) -> Forward-Fun))
(define ((monotone2d/fwd name f) ω γ bs)
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
       (values Ω (rect-intersect (pair-rect X Y) Γ))]
      [_
       (values empty-set empty-set)])))

(: monotone2d/comp (Omega-Idx Pair-Rect Interval
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              -> Computation))
(define (monotone2d/comp r f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (simple-computation/domain
   r f-domain
   (λ (Ω Γ bs)
     (define range (monotone2d-range Γ f-range f fx? fy?))
     (define pre (simple-preimage Ω Γ range (monotone2d/pre Ω Γ g gz? gy? h hz? hx?)))
     (computation-meaning bs range pre))))

(: monotone2d/arr (Symbol Pair-Rect Interval
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    -> expression))
(define (monotone2d/arr name f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty
                         (monotone2d/fwd name f)
                         (monotone2d/comp r f-domain f-range f fx? fy? g gz? gy? h hz? hx?)))))

;; ---------------------------------------------------------------------------------------------------
;; Concrete monotone R R -> R functions

(define +/arr
  (monotone2d/arr '+/arr (pair-rect reals reals) reals
                            fl+ #t #t
                            fl- #t #f
                            fl- #t #f))

(: neg-fl- (Flonum Flonum -> Flonum))
(define (neg-fl- z x) (fl- x z))

(define -/arr
  (monotone2d/arr '-/arr (pair-rect reals reals) reals
                            fl- #t #f
                            fl+ #t #t
                            neg-fl- #f #t))

(define pos-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-reals nonnegative-reals) nonnegative-reals
                            fl* #t #t
                            fl/ #t #f
                            fl/ #t #f))

(define pos-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-reals negative-reals) nonpositive-reals
                            fl* #f #t
                            fl/ #f #t
                            fl/ #t #t))

(define neg-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-reals nonnegative-reals) nonpositive-reals
                            fl* #t #f
                            fl/ #t #t
                            fl/ #f #t))

(define neg-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-reals negative-reals) positive-reals
                            fl* #f #f
                            fl/ #f #f
                            fl/ #f #f))

(: inv-fl/ (Flonum Flonum -> Flonum))
(define (inv-fl/ z x) (fl/ x z))

(define pos-pos-div/arr
  (monotone2d/arr '//arr (pair-rect positive-reals positive-reals) positive-reals
                            fl/ #t #f
                            fl* #t #t
                            inv-fl/ #f #t))

(define pos-neg-div/arr
  (monotone2d/arr '//arr (pair-rect positive-reals negative-reals) negative-reals
                            fl/ #f #f
                            fl* #f #f
                            inv-fl/ #f #f))

(define neg-pos-div/arr
  (monotone2d/arr '//arr (pair-rect negative-reals positive-reals) negative-reals
                            fl/ #t #t
                            fl* #t #f
                            inv-fl/ #t #f))

(define neg-neg-div/arr
  (monotone2d/arr '//arr (pair-rect negative-reals negative-reals) positive-reals
                            fl/ #f #t
                            fl* #f #t
                            inv-fl/ #t #t))

;; ---------------------------------------------------------------------------------------------------
;; Square

(: sqr/fwd Forward-Fun)
(define (sqr/fwd ω γ bs)
  (if (flonum? γ) (fl* γ γ) (raise-argument-error 'sqr/fwd "flonum" γ)))

(: sqr-range (Nonempty-Rect -> Rect))
(define (sqr-range domain)
  (match-define (interval a b a? b?) domain)
  (cond [(a . >= . 0.0)  (interval (* a a) (* b b) a? b?)]
        [(b . <= . 0.0)  (interval (* b b) (* a a) b? a?)]
        [else  (define c (* a a))
               (define d (* b b))
               (cond [(c . > . d)  (interval 0.0 c #t a?)]
                     [(d . > . c)  (interval 0.0 d #t b?)]
                     [else  (interval 0.0 c #t (or a? b?))])]))

(: sqr/pre (Omega-Rect Nonempty-Rect -> Simple-Preimage-Fun))
(define ((sqr/pre Ω Γ) B)
  (match B
    [(interval a b a? b?)
     (let-values ([(a a?)  (if (< a 0.0) (values 0.0 #t) (values a a?))]
                  [(b b?)  (if (< b 0.0) (values 0.0 #t) (values b b?))])
       (define A1 (interval (flsqrt a) (flsqrt b) a? b?))
       (define A2 (interval (- (flsqrt b)) (- (flsqrt a)) b? a?))
       (values Ω (rect-join (rect-intersect A1 Γ)
                            (rect-intersect A2 Γ))))]
    [_
     (values empty-set empty-set)]))

(: sqr/comp (Omega-Idx -> Computation))
(define (sqr/comp r)
  (simple-computation/domain
   r reals
   (λ (Ω Γ bs)
     (define range (sqr-range Γ))
     (computation-meaning bs range (simple-preimage Ω Γ range (sqr/pre Ω Γ))))))

(define sqr/arr
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty sqr/fwd (sqr/comp r)))))

;; ---------------------------------------------------------------------------------------------------
;; Predicates

(: predicate/fwd ((Value -> Boolean) -> Forward-Fun))
(define ((predicate/fwd pred?) ω γ bs)
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

(: predicate/comp (Omega-Idx Rect Rect -> Computation))
(define (predicate/comp r true-set false-set)
  (simple-computation/domain
   r (rect-join true-set false-set)
   (λ (Ω Γ bs)
     (let ([true-set   (rect-intersect Γ true-set)]
           [false-set  (rect-intersect Γ false-set)])
       (define range (predicate-range true-set false-set))
       (define pre (simple-preimage Ω Γ range (predicate/pre Ω true-set false-set)))
       (computation-meaning bs range pre)))))

(: predicate/arr ((Value -> Boolean) Nonempty-Rect Nonempty-Rect -> expression))
(define (predicate/arr pred? true-set false-set)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty
                         (predicate/fwd pred?)
                         (predicate/comp r true-set false-set)))))

(: real-predicate/arr (Symbol (Flonum -> Boolean) Interval Interval -> expression))
(define (real-predicate/arr name p? true-ivl false-ivl)
  (predicate/arr (λ: ([γ : Value])
                   (if (flonum? γ) (p? γ) (raise-argument-error name "Flonum" γ)))
                 true-ivl false-ivl))

(define negative?/arr
  (real-predicate/arr 'negative?/arr (λ: ([x : Flonum]) (x . fl< . 0.0))
                      negative-reals nonnegative-reals))

(define positive?/arr
  (real-predicate/arr 'positive?/arr (λ: ([x : Flonum]) (x . fl> . 0.0))
                      positive-reals nonpositive-reals))

(define nonpositive?/arr
  (real-predicate/arr 'nonpositive?/arr (λ: ([x : Flonum]) (x . fl<= . 0.0))
                      nonpositive-reals positive-reals))

(define nonnegative?/arr
  (real-predicate/arr 'nonnegative?/arr (λ: ([x : Flonum]) (x . fl>= . 0.0))
                      nonnegative-reals negative-reals))

(define null?/arr
  (predicate/arr null? null-rect (Join-Rect reals #f universal-pair 'tf)))

;; ---------------------------------------------------------------------------------------------------
;; Inequalities

(define lt/arr (ap/arr negative?/arr -/arr))
(define gt/arr (ap/arr positive?/arr -/arr))
(define lte/arr (ap/arr nonpositive?/arr -/arr))
(define gte/arr (ap/arr nonnegative?/arr -/arr))

;; ---------------------------------------------------------------------------------------------------
;; Conditionals

(define-predicate if-bad-branch? 'if-bad-branch)

(: if/fwd (Omega-Idx Forward-Fun (Promise Forward-Fun) (Promise Forward-Fun) -> Forward-Fun))
(define ((if/fwd r c-fwd t-fwd f-fwd) ω γ bs)
  (define c (c-fwd ω γ bs))
  (define b (branches-rect-ref bs r))
  (cond [(or (not (boolean? c)) (not (boolean-set-member? b c)))  (raise 'if-bad-branch)]
        [c     ((force t-fwd) ω γ bs)]
        [else  ((force f-fwd) ω γ bs)]))

(: if/comp (Boolean Omega-Idx Computation (Promise expression-meaning) (Promise expression-meaning)
                    -> Computation))
(define (if/comp strict? r c-comp t-meaning f-meaning)
  (simple-computation
   r
   (λ (Ω Γ bs)
     (match-let ([(computation-meaning bs c-range c-pre)  (c-comp Ω Γ bs)])
       (define-values (Ωt Γt)
         (cond [(rect-member? c-range #t)  (c-pre 't)]
               [else  (values empty-set empty-set)]))
       
       (define-values (Ωf Γf)
         (cond [(rect-member? c-range #f)  (c-pre 'f)]
               [else  (values empty-set empty-set)]))
       
       ;; t? = #t if it's possible to take the true branch
       ;; f? = #t if it's possible to take the false branch
       (define-values (t? f?)
         (let-values ([(bt? bf?)  (boolean-set->booleans (branches-rect-ref bs r))])
           (values (and bt? (not (empty-set? Ωt)) (not (empty-set? Γt)))
                   (and bf? (not (empty-set? Ωf)) (not (empty-set? Γf))))))
       
       (cond
         [(and t? f?)
          (define Ω (omega-rect-join Ωt Ωf))
          (define Γ (rect-join Γt Γf))
          (cond
            [strict?
             (define t-comp (expression-meaning-computation (force t-meaning)))
             (define f-comp (expression-meaning-computation (force f-meaning)))
             (match-let* ([(computation-meaning bs t-range t-pre)  (t-comp Ωt Γt bs)]
                          [(computation-meaning bs f-range f-pre)  (f-comp Ωf Γf bs)])
               (define range (rect-join t-range f-range))
               
               (: if/pre Simple-Preimage-Fun)
               (define (if/pre B)
                 (define Bt (rect-intersect t-range B))
                 (define Bf (rect-intersect f-range B))
                 (let-values ([(Ωt At)  (t-pre Bt)]
                              [(Ωf Af)  (f-pre Bf)])
                   (values (omega-rect-join Ωt Ωf) (rect-join At Af))))
               
               (computation-meaning bs range (simple-preimage Ω Γ range if/pre)))]
            [else
             (define range universal-set)
             (computation-meaning bs range (simple-preimage Ω Γ range (λ (B) (values Ω Γ))))])]
         [t?
          (define t-comp (expression-meaning-computation (force t-meaning)))
          (match-let ([(computation-meaning bs t-range t-pre)  (t-comp Ωt Γt bs)])
            (computation-meaning (branches-rect-restrict bs r 't) t-range t-pre))]
         [f?
          (define f-comp (expression-meaning-computation (force f-meaning)))
          (match-let ([(computation-meaning bs f-range f-pre)  (f-comp Ωf Γf bs)])
            (computation-meaning (branches-rect-restrict bs r 'f) f-range f-pre))]
         [else
          (bottom/comp empty-set empty-set bs)])))))

(: if/arr (Boolean -> (expression expression expression -> expression)))
(define ((if/arr strict?) c-expr t-expr f-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (define rte (omega-expr-idx r r1))
     (match-define (expression-meaning c-idxs c-fwd c-comp) (run-expression c-expr r0 r))
     (define t-meaning (delay (run-expression t-expr r rte)))
     (define f-meaning (delay (run-expression f-expr rte r1)))
     (define t-idxs (delay (expression-meaning-indexes (force t-meaning))))
     (define f-idxs (delay (expression-meaning-indexes (force f-meaning))))
     (expression-meaning
      (append c-idxs (list (if-indexes r t-idxs f-idxs)))
      (if/fwd r c-fwd
              (delay (expression-meaning-forward (force t-meaning)))
              (delay (expression-meaning-forward (force f-meaning))))
      (if/comp strict? r c-comp t-meaning f-meaning)))))

(define lazy-if/arr (if/arr #f))
(define strict-if/arr (if/arr #t))

(define */arr
  (strict-if/arr (ap/arr negative?/arr (ref/arr 'fst))
                 (strict-if/arr (ap/arr negative?/arr (ref/arr 'snd))
                                neg-neg-mul/arr
                                neg-pos-mul/arr)
                 (strict-if/arr (ap/arr negative?/arr (ref/arr 'snd))
                                pos-neg-mul/arr
                                pos-pos-mul/arr)))

(define inv/arr
  (strict-if/arr positive?/arr
                 pos-inv/arr
                 (strict-if/arr negative?/arr
                                neg-inv/arr
                                bottom/arr)))

(define //arr
  (strict-if/arr (ap/arr positive?/arr (ref/arr 'snd))
                 (strict-if/arr (ap/arr positive?/arr (ref/arr 'fst))
                                pos-pos-div/arr
                                (strict-if/arr (ap/arr negative?/arr (ref/arr 'fst))
                                               neg-pos-div/arr
                                               (c/arr 0.0)))
                 (strict-if/arr (ap/arr negative?/arr (ref/arr 'snd))
                                (strict-if/arr (ap/arr positive?/arr (ref/arr 'fst))
                                               pos-neg-div/arr
                                               (strict-if/arr (ap/arr negative?/arr (ref/arr 'fst))
                                                              neg-neg-div/arr
                                                              (c/arr 0.0)))
                                bottom/arr)))
