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

(define-type Forward-Fun (Value Branches-Rect -> Value))

(: run-expression (case-> (expression -> expression-meaning)
                          (expression Omega-Idx Omega-Idx -> expression-meaning)))
(define (run-expression e [r0 0] [r1 1])
  ((expression-fun e) r0 r1))

;; A computation is a function from a domain and branches to its meaning
(define-type Computation (Rect (U Empty-Set Branches-Rect) -> computation-meaning))

;; A computation means:
;;  1. A branches rectangle (which bounds the branches the forward computation can take)
;;  2. The approximate range of its forward function
;;  3. A function that computes approximate preimages under its forward function
(struct: computation-meaning ([branches : (U Empty-Set Branches-Rect)]
                              [range : Rect]
                              [preimage : Preimage-Fun])
  #:transparent)

(define-type Preimage-Fun (Rect -> Rect))

;; ===================================================================================================
;; Conveniences

(define-type Prim-Preimage-Fun (Nonempty-Rect -> Rect))

(: make-preimage (Rect Rect Prim-Preimage-Fun -> Preimage-Fun))
;; Wraps a Prim-Preimage-Fun with code that ensures the argument is a subset of the range and is
;; nonempty; also performs an optimization
(define ((make-preimage domain range pre) B)
  (let ([B  (rect-intersect B range)])
    (cond [(empty-set? B)  empty-set]
          [(eq? B range)   domain]  ; optimization
          [else  (pre B)])))

(: make-expression ((Value Branches-Rect -> Value) Computation -> expression))
(define (make-expression fwd comp)
  (expression (λ (r0 r1) (expression-meaning empty fwd comp))))

(define-type Prim-Computation (Nonempty-Rect Branches-Rect -> computation-meaning))

(struct: comp-rec ([domain : (U #f Nonempty-Rect)]
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

(: make-computation (Omega-Idx Prim-Computation -> Computation))
(define (make-computation r prim/comp)
  ;(define rec (hash-ref! comp-hash r (λ () (comp-rec #f #f #f))))
  (λ (domain bs)
    (cond [(or (empty-set? domain) (empty-set? bs))  (bottom/comp domain bs)]
          [else
           ;(match-define (comp-rec last-domain last-bs last-meaning) rec)
           (cond #;[(and (eq? domain last-domain) (eq? bs last-bs) last-meaning)
                  (set! comp-hash-hits (+ 1 comp-hash-hits))
                  last-meaning]
                 [else
                  ;(set! comp-hash-misses (+ 1 comp-hash-misses))
                  (define m (prim/comp domain bs))
                  ;(set-comp-rec-domain! rec domain)
                  ;(set-comp-rec-branches! rec bs)
                  ;(set-comp-rec-meaning! rec m)
                  m])])))

(: make-computation/domain (Omega-Idx Rect Prim-Computation -> Computation))
(define (make-computation/domain r prim-domain prim/comp)
  (cond [(empty-set? prim-domain)  bottom/comp]
        [else
         (define rec (hash-ref! comp-hash r (λ () (comp-rec #f #f #f))))
         (λ (domain bs)
           (let ([domain  (rect-intersect domain prim-domain)])
             (cond [(or (empty-set? domain) (empty-set? bs))  (bottom/comp domain bs)]
                   [else
                    (match-define (comp-rec last-domain last-bs last-meaning) rec)
                    (cond [(and (eq? domain last-domain) (eq? bs last-bs) last-meaning)
                           (set! comp-hash-hits (+ 1 comp-hash-hits))
                           last-meaning]
                          [else
                           (set! comp-hash-misses (+ 1 comp-hash-misses))
                           (define m (prim/comp domain bs))
                           (set-comp-rec-domain! rec domain)
                           (set-comp-rec-branches! rec bs)
                           (set-comp-rec-meaning! rec m)
                           m])])))]))

;; ===================================================================================================
;; Basic primitives

;; Empty function (aka bottom; i.e. has empty range)

(: bottom/fwd Forward-Fun)
(define (bottom/fwd γ bs)
  (error 'bottom/arr "empty range"))

(: bottom/pre Preimage-Fun)
(define (bottom/pre B) empty-set)

(: bottom/comp Computation)
(define (bottom/comp domain bs)
  (computation-meaning bs empty-set bottom/pre))

(define bottom/arr
  (make-expression bottom/fwd bottom/comp))

;; Identity function

(: id/fwd Forward-Fun)
(define (id/fwd γ bs) γ)

(: id/pre Preimage-Fun)
(define id/pre identity)

(: id/comp (Omega-Idx -> Computation))
(define (id/comp r)
  (make-computation
   r (λ (domain bs)
       (computation-meaning bs domain id/pre))))

(define id/arr
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty id/fwd (id/comp r)))))

;; Constant functions

(: c/fwd (Value -> Forward-Fun))
(define ((c/fwd x) γ bs) x)

(: c/pre (Rect -> Preimage-Fun))
(define ((c/pre domain) B)
  (if (empty-set? B) empty-set domain))

(: c/comp (Omega-Idx Value -> Computation))
(define (c/comp r x)
  (define X (value->singleton x))
  (make-computation
   r (λ (domain bs)
       (computation-meaning bs X (c/pre domain)))))

(: c/arr (Value -> expression))
(define (c/arr x)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty (c/fwd x) (c/comp r x)))))

;; ---------------------------------------------------------------------------------------------------
;; Application

(: ap/fwd (Forward-Fun Forward-Fun -> Forward-Fun))
(define ((ap/fwd f-fwd g-fwd) γ bs)
  (let* ([γ  (g-fwd γ bs)]
         [γ  (f-fwd γ bs)])
    γ))

(: ap/pre (Preimage-Fun Preimage-Fun -> Prim-Preimage-Fun))
(define ((ap/pre f-pre g-pre) C)
  (let* ([B  (f-pre C)]
         [A  (g-pre B)])
    A))

(: ap/comp (Omega-Idx Computation Computation -> Computation))
(define (ap/comp r f-comp g-comp)
  (make-computation
   r (λ (domain bs)
       (match-let* ([(computation-meaning bs g-range g-pre)  (g-comp domain bs)]
                    [(computation-meaning bs f-range f-pre)  (f-comp g-range bs)])
         (computation-meaning bs f-range (make-preimage domain f-range (ap/pre f-pre g-pre)))))))

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
(define ((rap/fwd f-fwd g-fwd) γ bs)
  (let* ([γ  (f-fwd γ bs)]
         [γ  (g-fwd γ bs)])
    γ))

(: rap/pre (Preimage-Fun Preimage-Fun -> Prim-Preimage-Fun))
(define ((rap/pre f-pre g-pre) C)
  (let* ([B  (g-pre C)]
         [A  (f-pre B)])
    A))

(: rap/comp (Omega-Idx Computation Computation -> Computation))
(define (rap/comp r f-comp g-comp)
  (make-computation
   r (λ (domain bs)
       (match-let* ([(computation-meaning bs f-range f-pre)  (f-comp domain bs)]
                    [(computation-meaning bs g-range g-pre)  (g-comp f-range bs)])
         (computation-meaning bs g-range (make-preimage domain g-range (rap/pre f-pre g-pre)))))))

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
(define ((pair/fwd fst-fwd snd-fwd) γ bs)
  (cons (fst-fwd γ bs) (snd-fwd γ bs)))

(: pair/pre (Preimage-Fun Preimage-Fun Nonempty-Rect Rect Rect -> Prim-Preimage-Fun))
(define ((pair/pre fst-pre snd-pre domain fst-range snd-range) B1×B2)
  (match-define (pair-rect B1 B2) B1×B2)
  (define fst? (not (eq? B1 fst-range)))
  (define snd? (not (eq? B2 snd-range)))
  (cond [(and fst? snd?)  (rect-intersect (fst-pre B1) (snd-pre B2))]
        [fst?  (fst-pre B1)]
        [snd?  (snd-pre B2)]
        [else  domain]))

(: pair/comp (Omega-Idx Computation Computation -> Computation))
(define (pair/comp r fst-comp snd-comp)
  (make-computation
   r (λ (domain bs)
       (match-let* ([(computation-meaning bs fst-range fst-pre)  (fst-comp domain bs)]
                    [(computation-meaning bs snd-range snd-pre)  (snd-comp domain bs)])
         (define range (pair-rect fst-range snd-range))
         (define pre (pair/pre fst-pre snd-pre domain fst-range snd-range))
         (computation-meaning bs range (make-preimage domain range pre))))))

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
(define ((random/fwd r) γ bs)
  (value-ref γ r))

(: random/pre (Nonempty-Rect Omega-Idx -> Prim-Preimage-Fun))
(define ((random/pre domain r) B)
  (rect-set domain r B))

(: random/comp (Omega-Idx -> Computation))
(define (random/comp r)
  (make-computation
   r (λ (domain bs)
       (define range (rect-ref domain r))
       (computation-meaning bs range (make-preimage domain range (random/pre domain r))))))

(: random/arr expression)
(define random/arr
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning (list r) (random/fwd r) (random/comp r)))))

;; ---------------------------------------------------------------------------------------------------
;; Random boolean

(: random-boolean/fwd (Omega-Idx Flonum -> Forward-Fun))
(define ((random-boolean/fwd r p) γ bs)
  (cond [(omega? γ)  ((omega-ref γ r) . < . p)]
        [else  (raise-argument-error 'random-boolean/arr "Omega" γ)]))

(: random-boolean/pre (Nonempty-Rect Omega-Idx Rect Rect -> Prim-Preimage-Fun))
(define ((random-boolean/pre domain r It If) B)
  (define I (case B
              [(tf)  (rect-join It If)]
              [(t)   It]
              [(f)   If]
              [else  empty-set]))
  (rect-set domain r I))

(: random-boolean/comp (Omega-Idx Rect Rect -> Computation))
(define (random-boolean/comp r It If)
  (make-computation
   r (λ (domain bs)
       (define I (rect-ref domain r))
       (let ([It  (rect-intersect I It)]
             [If  (rect-intersect I If)])
         (define range (booleans->boolean-set (not (empty-set? It)) (not (empty-set? If))))
         (define pre (random-boolean/pre domain r It If))
         (computation-meaning bs range (make-preimage domain range pre))))))

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
(define ((ref/fwd j) γ bs)
  (value-ref γ j))

(: ref/pre (Nonempty-Rect Idx -> Prim-Preimage-Fun))
(define ((ref/pre domain j) B)
  (rect-set domain j B))

(: ref/comp (Omega-Idx Idx -> Computation))
(define (ref/comp r j)
  (make-computation
   r (λ (domain bs)
       (define range (rect-ref domain j))
       (computation-meaning bs range (make-preimage domain range (ref/pre domain j))))))

(: ref/arr (Idx -> expression))
(define (ref/arr j)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty (ref/fwd j) (ref/comp r j)))))

;; ---------------------------------------------------------------------------------------------------
;; Monotone R -> R functions

(: monotone/fwd (Symbol (Flonum -> Flonum) -> Forward-Fun))
(define ((monotone/fwd name f) γ bs)
  (if (flonum? γ) (f γ) (raise-argument-error name "Flonum" γ)))

(: monotone-range (Nonempty-Rect Interval (Flonum -> Flonum) Boolean -> Rect))
(define (monotone-range domain f-range f fx?)
  (match-define (interval a b a? b?) domain)
  (cond [fx?   (rect-intersect (interval (f a) (f b) a? b?) f-range)]
        [else  (rect-intersect (interval (f b) (f a) b? a?) f-range)]))

(: monotone/pre (Nonempty-Rect (Flonum -> Flonum) Boolean -> Prim-Preimage-Fun))
(define ((monotone/pre domain g fx?) B)
  (match B
    [(interval a b a? b?)
     (cond [fx?   (rect-intersect (interval (g a) (g b) a? b?) domain)]
           [else  (rect-intersect (interval (g b) (g a) b? a?) domain)])]
    [_  empty-set]))

(: monotone/comp (Omega-Idx Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                            -> Computation))
(define (monotone/comp r f-domain f-range f g fx?)
  (make-computation/domain
   r f-domain
   (λ (domain bs)
     (define range (monotone-range domain f-range f fx?))
     (computation-meaning bs range (make-preimage domain range (monotone/pre domain g fx?))))))

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
;; Concrete R -> R functions

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
(define ((monotone2d/fwd name f) γ bs)
  (match γ
    [(cons (? flonum? x) (? flonum? y))  (f x y)]
    [_  (raise-argument-error name "(Pair Flonum Flonum)" γ)]))

(: monotone2d-range (Nonempty-Rect Interval (Flonum Flonum -> Flonum) Boolean Boolean -> Rect))
(define (monotone2d-range domain f-range f fx? fy?)
  (match-define (pair-rect (interval xa xb xa? xb?) (interval ya yb ya? yb?)) domain)
  (let-values ([(xa xb xa? xb?)  (if fx? (values xa xb xa? xb?) (values xb xa xb? xa?))]
               [(ya yb ya? yb?)  (if fy? (values ya yb ya? yb?) (values yb ya yb? ya?))])
    (rect-intersect f-range (interval (f xa ya) (f xb yb) (and xa? ya?) (and xb? yb?)))))

(: monotone2d/pre (Nonempty-Rect (Flonum Flonum -> Flonum) Boolean Boolean
                                 (Flonum Flonum -> Flonum) Boolean Boolean
                                 -> Prim-Preimage-Fun))
(define (monotone2d/pre domain g gz? gy? h hz? hx?)
  (match-define (pair-rect (interval xa xb xa? xb?) (interval ya yb ya? yb?)) domain)
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
       (rect-intersect (pair-rect X Y) domain)]
      [_  empty-set])))

(: monotone2d/comp (Omega-Idx Pair-Rect Interval
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              (Flonum Flonum -> Flonum) Boolean Boolean
                              -> Computation))
(define (monotone2d/comp r f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (make-computation/domain
   r f-domain
   (λ (domain bs)
     (define range (monotone2d-range domain f-range f fx? fy?))
     (define pre (make-preimage domain range (monotone2d/pre domain g gz? gy? h hz? hx?)))
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
(define (sqr/fwd γ bs)
  (if (flonum? γ) (fl* γ γ) (raise-argument-error 'sqr/env "flonum" γ)))

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

(: sqr/pre (Nonempty-Rect -> Prim-Preimage-Fun))
(define ((sqr/pre domain) B)
  (match B
    [(interval a b a? b?)
     (let-values ([(a a?)  (if (< a 0.0) (values 0.0 #t) (values a a?))]
                  [(b b?)  (if (< b 0.0) (values 0.0 #t) (values b b?))])
       (define A1 (interval (flsqrt a) (flsqrt b) a? b?))
       (define A2 (interval (- (flsqrt b)) (- (flsqrt a)) b? a?))
       (rect-join (rect-intersect A1 domain)
                  (rect-intersect A2 domain)))]
    [_  empty-set]))

(: sqr/comp (Omega-Idx -> Computation))
(define (sqr/comp r)
  (make-computation/domain
   r reals
   (λ (domain bs)
     (define range (sqr-range domain))
     (computation-meaning bs range (make-preimage domain range (sqr/pre domain))))))

(define sqr/arr
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (expression-meaning empty sqr/fwd (sqr/comp r)))))

;; ---------------------------------------------------------------------------------------------------
;; Predicates

(: predicate/fwd ((Value -> Boolean) -> Forward-Fun))
(define ((predicate/fwd pred?) γ bs)
  (pred? γ))

(: predicate-range (Rect Rect -> (U Empty-Set Boolean-Set)))
(define (predicate-range true-set false-set)
  (booleans->boolean-set (not (empty-set? true-set))
                         (not (empty-set? false-set))))

(: predicate/pre (Rect Rect -> Prim-Preimage-Fun))
(define ((predicate/pre true-set false-set) B)
  (case B
    [(tf)  (rect-join true-set false-set)]
    [(t)   true-set]
    [(f)   false-set]
    [else  empty-set]))

(: predicate/comp (Omega-Idx Rect Rect -> Computation))
(define (predicate/comp r true-set false-set)
  (make-computation/domain
   r (rect-join true-set false-set)
   (λ (domain bs)
     (let ([true-set   (rect-intersect domain true-set)]
           [false-set  (rect-intersect domain false-set)])
       (define range (predicate-range true-set false-set))
       (define pre (make-preimage domain range (predicate/pre true-set false-set)))
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
(define ((if/fwd r c-fwd t-fwd f-fwd) γ bs)
  (define c (c-fwd γ bs))
  (define b (branches-rect-ref bs r))
  (cond [(or (not (boolean? c)) (not (boolean-set-member? b c)))  (raise 'if-bad-branch)]
        [c     ((force t-fwd) γ bs)]
        [else  ((force f-fwd) γ bs)]))

(: if/comp (Boolean Omega-Idx Computation (Promise expression-meaning) (Promise expression-meaning)
                    -> Computation))
(define (if/comp strict? r c-comp t-meaning f-meaning)
  (make-computation
   r
   (λ (domain bs)
     (match-let ([(computation-meaning bs c-range c-pre)  (c-comp domain bs)])
       (define t-domain (if (rect-member? c-range #t) (c-pre 't) empty-set))
       (define f-domain (if (rect-member? c-range #f) (c-pre 'f) empty-set))
       
       ;; t? = #t if it's possible to take the true branch
       ;; f? = #t if it's possible to take the false branch
       (define-values (t? f?)
         (let-values ([(bt? bf?)  (boolean-set->booleans (branches-rect-ref bs r))])
           (values (and bt? (not (empty-set? t-domain)))
                   (and bf? (not (empty-set? f-domain))))))
       
       (cond
         [(and t? f?)
          (define domain (rect-join t-domain f-domain))
          (cond
            [strict?
             (define t-comp (expression-meaning-computation (force t-meaning)))
             (define f-comp (expression-meaning-computation (force f-meaning)))
             (match-let* ([(computation-meaning bs t-range t-pre)  (t-comp t-domain bs)]
                          [(computation-meaning bs f-range f-pre)  (f-comp f-domain bs)])
               (define range (rect-join t-range f-range))
               
               (: if/pre Prim-Preimage-Fun)
               (define (if/pre B)
                 (define Bt (rect-intersect t-range B))
                 (define Bf (rect-intersect f-range B))
                 (rect-join (t-pre Bt) (f-pre Bf)))
               
               (computation-meaning bs range (make-preimage domain range if/pre)))]
            [else
             (define range universal-set)
             (computation-meaning bs range (make-preimage domain range (λ (B) domain)))])]
         [t?
          (define t-comp (expression-meaning-computation (force t-meaning)))
          (match-let ([(computation-meaning bs t-range t-pre)  (t-comp t-domain bs)])
            (computation-meaning (branches-rect-restrict bs r 't) t-range t-pre))]
         [f?
          (define f-comp (expression-meaning-computation (force f-meaning)))
          (match-let ([(computation-meaning bs f-range f-pre)  (f-comp f-domain bs)])
            (computation-meaning (branches-rect-restrict bs r 'f) f-range f-pre))]
         [else
          (bottom/comp empty-set bs)])))))

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
