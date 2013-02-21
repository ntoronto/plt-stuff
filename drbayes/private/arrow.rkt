#lang typed/racket/base

#|
TODO

Fix endpoint booleans sent to interval in +/pre
|#

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

(define-type Preimage-Fun (Nonempty-Rect Branches-Rect Nonempty-Rect
                                         -> (Rect))

(struct: function ([range : Rect]
                   [branches : (U Empty-Set Branch-Rect)]
                   [preimage : (Rect (U Empty-Set Branches-Rect) 
                                     -> (Values Rect (U Empty-Set Branches-Rect)))])
  #:transparent)

(: make-function (Rect Rect Preimage-Fun -> function))
(define (make-function domain range pre)
  (function domain range
            (λ (B)
              (let ([B  (rect-intersect B range)])
                (cond [(empty-set? B)  empty-set]
                      [(eq? B range)   domain]
                      [else  (pre B)])))))

(define-type Prim-Computation (Nonempty-Rect Branches -> function))
(define-type Computation (Rect Branches -> function))

(struct: meaning ([indexes : Indexes]
                  [forward : (Value Branches -> Value)]
                  [computation : Computation])
  #:transparent)

(struct: expression ([value : (Omega-Idx Omega-Idx -> meaning)])
  #:transparent)

(: run-expression (case-> (expression -> meaning)
                          (expression Omega-Idx Omega-Idx -> meaning)))
(define (run-expression e [r0 0] [r1 1])
  ((expression-value e) r0 r1))

(: apply-preimage (function Rect -> Rect))
(define (apply-preimage e B)
  ((function-preimage e) B))
#|
;; ===================================================================================================
;; Basic primitives

(: c/comp (Value -> Computation))
(define (c/comp x)
  (define X (value->singleton x))
  (λ (domain bs)
    (define range (if (empty-set? domain) empty-set X))
    
    (: c/pre (Nonempty-Rect -> Rect))
    (define (c/pre B) domain)
    
    (make-function domain empty range c/pre)))

(: empty/comp Computation)
(define (empty/comp domain bs)
  
  (: empty/pre (Nonempty-Rect -> Rect))
  (define (empty/pre B) empty-set)
  
  (make-function domain empty empty-set empty/pre))

(: make-primitive (case-> ((Value Branches -> Value) Computation -> expression)
                          ((Value Branches -> Value) Prim-Computation Nonempty-Rect -> expression)))
(define make-primitive
  (case-lambda
    [(fwd comp)  (expression (λ (r0 r1) (meaning empty fwd comp)))]
    [(fwd prim-comp prim-domain)
     (make-primitive
      fwd
      (λ: ([domain : Rect] [bs : Branches])
        (let ([domain  (rect-intersect domain prim-domain)])
          (cond [(empty-set? domain)  (empty/comp domain bs)]
                [else  (prim-comp domain bs)]))))]))

(define id/arr
  (make-primitive
   (λ: ([γ : Value] [bs : Branches]) γ)
   (λ: ([domain : Rect] [bs : Branches])
     (make-function domain empty domain identity))))

(: c/arr (Value -> expression))
(define (c/arr x) 
  (make-primitive
   (λ (γ bs) x)
   (c/comp x)))

(: empty/arr expression)
(define empty/arr
  (make-primitive
   (λ (γ bs) (error 'empty/arr "empty range"))
   empty/comp))

;; ---------------------------------------------------------------------------------------------------
;; Application

(: ap/arr (expression expression -> expression))
(define (ap/arr f-expr g-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (match-define (meaning g-idxs g-fwd g-comp) (run-expression g-expr r r1))
     (match-define (meaning f-idxs f-fwd f-comp) (run-expression f-expr r0 r))
     (meaning
      (append f-idxs g-idxs)
      (λ (γ bs)
        (let* ([γ  (g-fwd γ bs)]
               [γ  (f-fwd γ bs)])
          γ))
      (λ (domain bs)
        (define g (g-comp domain bs))
        (define f (f-comp (function-range g) bs))
        (define range (function-range f))
        
        (: ap/pre (Nonempty-Rect -> Rect))
        (define (ap/pre C)
          (let* ([B  (apply-preimage f C)]
                 [A  (apply-preimage g B)])
            A))
        
        (define new-bs (append (function-new-branches g) (function-new-branches f)))
        
        (make-function domain new-bs range ap/pre))))))

(: rap/arr (expression expression -> expression))
(define (rap/arr f-expr g-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (match-define (meaning f-idxs f-fwd f-comp) (run-expression f-expr r0 r))
     (match-define (meaning g-idxs g-fwd g-comp) (run-expression g-expr r r1))
     (meaning
      (append f-idxs g-idxs)
      (λ (γ bs)
        (let* ([γ  (f-fwd γ bs)]
               [γ  (g-fwd γ bs)])
          γ))
      (λ (domain bs)
        (define f (f-comp domain bs))
        (define g (g-comp (function-range f) bs))
        (define range (function-range g))
        
        (: rap/pre (Nonempty-Rect -> Rect))
        (define (rap/pre C)
          (let* ([B  (apply-preimage g C)]
                 [A  (apply-preimage f B)])
            A))
        
        (define new-bs (append (function-new-branches f) (function-new-branches g)))
        
        (make-function domain new-bs range rap/pre))))))

;; ---------------------------------------------------------------------------------------------------
;; Pairs and lists

(define null/arr (c/arr null))

(: list/arr (expression * -> expression))
(define (list/arr . es)
  (foldr pair/arr null/arr es))

(: pair/arr (expression expression -> expression))
(define (pair/arr fst-expr snd-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (match-define (meaning fst-idxs fst-fwd fst-comp) (run-expression fst-expr r0 r))
     (match-define (meaning snd-idxs snd-fwd snd-comp) (run-expression snd-expr r r1))
     (meaning
      (append fst-idxs snd-idxs)
      (λ (γ bs)
        (cons (fst-fwd γ bs)
              (snd-fwd γ bs)))
      (λ (domain bs)
        (define fst (fst-comp domain bs))
        (define snd (snd-comp domain bs))
        (define range-B1 (function-range fst))
        (define range-B2 (function-range snd))
        (define range (pair-rect range-B1 range-B2))
        
        (: pair/pre (Nonempty-Rect -> Rect))
        (define (pair/pre B1×B2)
          (match B1×B2
            [(pair-rect B1 B2)
             (define fst? (not (eq? B1 range-B1)))
             (define snd? (not (eq? B2 range-B2)))
             (cond [(and fst? snd?)  (rect-intersect (apply-preimage fst B1)
                                                     (apply-preimage snd B2))]
                   [fst?  (apply-preimage fst B1)]
                   [snd?  (apply-preimage snd B2)]
                   [else  domain])]
            [_  empty-set]))
        
        (define new-bs (append (function-new-branches fst) (function-new-branches snd)))
        
        (make-function domain new-bs range pair/pre))))))

;; ---------------------------------------------------------------------------------------------------
;; Random

(: random/arr expression)
(define random/arr
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (meaning
      (list r)
      (λ (γ bs) (value-ref γ r))
      (λ (domain bs)
        (define range (rect-ref domain r))
        
        (: random/pre (Nonempty-Rect -> Rect))
        (define (random/pre B) (rect-set domain r B))
        
        (make-function domain empty range random/pre))))))

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
        (meaning
         (list (cons r split))
         (λ (γ bs)
           (cond [(omega? γ)  ((omega-ref γ r) . < . p)]
                 [else  (raise-argument-error 'random-boolean/arr "Omega" γ)]))
         (λ (domain bs)
           (define I (rect-ref domain r))
           (let ([It  (rect-intersect I It)]
                 [If  (rect-intersect I If)])
             (define range (booleans->boolean-set (not (empty-set? It)) (not (empty-set? If))))
             
             (: random-boolean/pre (Nonempty-Rect -> Rect))
             (define (random-boolean/pre B)
               (define I (case B
                           [(tf)  (rect-join It If)]
                           [(t)   It]
                           [(f)   If]
                           [else  empty-set]))
               (rect-set domain r I))
             
             (make-function domain empty range random-boolean/pre))))))]
    [(= p 0.0)  (c/arr #f)]
    [(= p 1.0)  (c/arr #t)]
    [else  (raise-argument-error 'boolean "probability" p)]))

;; ===================================================================================================
;; Primitives

;; ---------------------------------------------------------------------------------------------------
;; Ref

(: ref/arr (Idx -> expression))
(define (ref/arr j)
  (make-primitive
   (λ: ([γ : Value] [bs : Branches])
     (value-ref γ j))
   (λ: ([domain : Rect] [bs : Branches])
     (define range (rect-ref domain j))
     
     (: ref/pre (Nonempty-Rect -> Rect))
     (define (ref/pre B) (rect-set domain j B))
     
     (make-function domain empty range ref/pre))))

;; ---------------------------------------------------------------------------------------------------
;; Monotone R -> R functions

(: monotone-function/arr (Symbol Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                                 -> expression))
(define (monotone-function/arr name f-domain f-range f g inc?)
  (make-primitive
   (λ: ([γ : Value] [bs : Branches])
     (if (flonum? γ) (f γ) (raise-argument-error name "Flonum" γ)))
   (λ: ([domain : Nonempty-Rect] [bs : Branches])
     (match-define (interval a b a? b?) domain)
     (define range
       (cond [inc?  (rect-intersect (interval (f a) (f b) a? b?) f-range)]
             [else  (rect-intersect (interval (f b) (f a) b? a?) f-range)]))
     
     (: f/pre (Nonempty-Rect -> Rect))
     (define (f/pre B)
       (match B
         [(interval a b a? b?)
          (cond [inc?  (rect-intersect (interval (g a) (g b) a? b?) domain)]
                [else  (rect-intersect (interval (g b) (g a) b? a?) domain)])]
         [_  empty-set]))
     
     (make-function domain empty range f/pre))
   f-domain))

(: scale/arr (Flonum -> expression))
(define (scale/arr y)
  (cond [(fl= y 0.0)  (c/arr 0.0)]
        [else  (monotone-function/arr 'scale/arr reals reals
                                      (λ: ([x : Flonum]) (fl* x y))
                                      (λ: ([z : Flonum]) (fl/ z y))
                                      (y . fl> . 0.0))]))

(: translate/arr (Flonum -> expression))
(define (translate/arr y)
  (monotone-function/arr 'translate/arr reals reals
                         (λ: ([x : Flonum]) (fl+ x y))
                         (λ: ([z : Flonum]) (fl- z y))
                         #t))

(define neg/arr (scale/arr -1.0))

(define exp/arr (monotone-function/arr 'exp/arr reals nonnegative-reals flexp fllog #t))
(define log/arr (monotone-function/arr 'log/arr nonnegative-reals reals fllog flexp #t))
(define sqrt/arr (monotone-function/arr
                  'sqrt/arr nonnegative-reals nonnegative-reals flsqrt (λ (x) (* x x)) #t))

(: flinv (Flonum -> Flonum))
(define (flinv x) (fl/ 1.0 x))

(define pos-inv/arr (monotone-function/arr 'inv/arr positive-reals positive-reals flinv flinv #t))
(define neg-inv/arr (monotone-function/arr 'inv/arr negative-reals negative-reals flinv flinv #f))

;; ---------------------------------------------------------------------------------------------------
;; Distributions from inverse cdf

(: inverse-cdf/arr (Symbol Interval (Flonum -> Flonum) (Flonum -> Flonum) -> expression))
(define (inverse-cdf/arr name range inv-cdf cdf)
  (monotone-function/arr name unit-interval range inv-cdf cdf #t))

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

(: monotone-2d-function/arr (Symbol Pair-Rect Interval
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    (Flonum Flonum -> Flonum) Boolean Boolean
                                    -> expression))
(define (monotone-2d-function/arr name f-domain f-range
                                  f fxinc? fyinc?
                                  g gzinc? gyinc?
                                  h hzinc? hxinc?)
  (make-primitive
   (λ: ([γ : Value] [bs : Branches])
     (match γ
       [(cons (? flonum? x) (? flonum? y))  (f x y)]
       [_  (raise-argument-error name "(Pair Flonum Flonum)" γ)]))
   (λ: ([domain : Nonempty-Rect] [bs : Branches])
     (match-define (pair-rect (interval xa xb xa? xb?) (interval ya yb ya? yb?)) domain)
     
     (define range
       (let-values ([(xa xb xa? xb?)  (if fxinc? (values xa xb xa? xb?) (values xb xa xb? xa?))]
                    [(ya yb ya? yb?)  (if fyinc? (values ya yb ya? yb?) (values yb ya yb? ya?))])
         (rect-intersect f-range (interval (f xa ya) (f xb yb) (and xa? ya?) (and xb? yb?)))))
     
     (: f/pre (Nonempty-Rect -> Rect))
     (define (f/pre B)
       (match B
         [(interval za zb za? zb?)
          (rect-intersect
           (pair-rect
            (let-values ([(za zb za? zb?)  (if gzinc? (values za zb za? zb?) (values zb za zb? za?))]
                         [(ya yb ya? yb?)  (if gyinc? (values ya yb ya? yb?) (values yb ya yb? ya?))])
              (interval (g za ya) (g zb yb) (and za? ya?) (and zb? yb?)))
            (let-values ([(za zb za? zb?)  (if hzinc? (values za zb za? zb?) (values zb za zb? za?))]
                         [(xa xb xa? xb?)  (if hxinc? (values xa xb xa? xb?) (values xb xa xb? xa?))])
              (interval (h za xa) (h zb xb) (and za? xa?) (and zb? xb?))))
           domain)]
         [_  empty-set]))
     
     (make-function domain empty range f/pre))
   f-domain))

(define +/arr
  (monotone-2d-function/arr '+/arr (pair-rect reals reals) reals
                            fl+ #t #t
                            fl- #t #f
                            fl- #t #f))

(: neg-fl- (Flonum Flonum -> Flonum))
(define (neg-fl- z x) (fl- x z))

(define -/arr
  (monotone-2d-function/arr '-/arr (pair-rect reals reals) reals
                            fl- #t #f
                            fl+ #t #t
                            neg-fl- #f #t))

(define pos-pos-mul/arr
  (monotone-2d-function/arr '*/arr (pair-rect nonnegative-reals nonnegative-reals) nonnegative-reals
                            fl* #t #t
                            fl/ #t #f
                            fl/ #t #f))

(define pos-neg-mul/arr
  (monotone-2d-function/arr '*/arr (pair-rect nonnegative-reals negative-reals) nonpositive-reals
                            fl* #f #t
                            fl/ #f #t
                            fl/ #t #t))

(define neg-pos-mul/arr
  (monotone-2d-function/arr '*/arr (pair-rect negative-reals nonnegative-reals) nonpositive-reals
                            fl* #t #f
                            fl/ #t #t
                            fl/ #f #t))

(define neg-neg-mul/arr
  (monotone-2d-function/arr '*/arr (pair-rect negative-reals negative-reals) positive-reals
                            fl* #f #f
                            fl/ #f #f
                            fl/ #f #f))

(: inv-fl/ (Flonum Flonum -> Flonum))
(define (inv-fl/ z x) (fl/ x z))

(define pos-pos-div/arr
  (monotone-2d-function/arr '//arr (pair-rect positive-reals positive-reals) positive-reals
                            fl/ #t #f
                            fl* #t #t
                            inv-fl/ #f #t))

(define pos-neg-div/arr
  (monotone-2d-function/arr '//arr (pair-rect positive-reals negative-reals) negative-reals
                            fl/ #f #f
                            fl* #f #f
                            inv-fl/ #f #f))

(define neg-pos-div/arr
  (monotone-2d-function/arr '//arr (pair-rect negative-reals positive-reals) negative-reals
                            fl/ #t #t
                            fl* #t #f
                            inv-fl/ #t #f))

(define neg-neg-div/arr
  (monotone-2d-function/arr '//arr (pair-rect negative-reals negative-reals) positive-reals
                            fl/ #f #t
                            fl* #f #t
                            inv-fl/ #t #t))

;; ---------------------------------------------------------------------------------------------------
;; Square

(define sqr/arr
  (make-primitive
   (λ: ([γ : Value] [bs : Branches])
     (cond [(flonum? γ)  (fl* γ γ)]
           [else  (raise-argument-error 'sqr/env "flonum" γ)]))
   (λ: ([domain : Nonempty-Rect] [bs : Branches])
     (match-define (interval a b a? b?) domain)
     (define range
       (cond [(a . >= . 0.0)  (interval (* a a) (* b b) a? b?)]
             [(b . <= . 0.0)  (interval (* b b) (* a a) b? a?)]
             [else  (define c (* a a))
                    (define d (* b b))
                    (cond [(c . > . d)  (interval 0.0 c #t a?)]
                          [(d . > . c)  (interval 0.0 d #t b?)]
                          [else  (interval 0.0 c #t (or a? b?))])]))
     
     (: sqr/pre (Nonempty-Rect -> Rect))
     (define (sqr/pre B)
       (match B
         [(interval a b a? b?)
          (let-values ([(a a?)  (if (< a 0.0) (values 0.0 #t) (values a a?))]
                       [(b b?)  (if (< b 0.0) (values 0.0 #t) (values b b?))])
            (define A1 (interval (flsqrt a) (flsqrt b) a? b?))
            (define A2 (interval (- (flsqrt b)) (- (flsqrt a)) b? a?))
            (rect-join (rect-intersect A1 domain)
                       (rect-intersect A2 domain)))]
         [_  empty-set]))
     
     (make-function domain empty range sqr/pre))
   reals))

;; ---------------------------------------------------------------------------------------------------
;; Predicates

(: predicate/arr ((Value -> Boolean) Nonempty-Rect Nonempty-Rect -> expression))
(define (predicate/arr pred? true-set false-set)
  (make-primitive
   (λ: ([γ : Value] [bs : Branches]) (pred? γ))
   (λ: ([domain : Nonempty-Rect] [bs : Branches])
     (let ([true-set   (rect-intersect domain true-set)]
           [false-set  (rect-intersect domain false-set)])
       
       (define range
         (booleans->boolean-set (not (empty-set? true-set))
                                (not (empty-set? false-set))))
       
       (: pred?/pre (Nonempty-Rect -> Rect))
       (define (pred?/pre B)
         (case B
           [(tf)  (rect-join true-set false-set)]
           [(t)   true-set]
           [(f)   false-set]
           [else  empty-set]))
       
       (make-function domain empty range pred?/pre)))
   (rect-join true-set false-set)))

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

(: if/arr (Boolean -> (expression expression expression -> expression)))
(define ((if/arr strict?) c-expr t-expr f-expr)
  (expression
   (λ (r0 r1)
     (define r (omega-expr-idx r0 r1))
     (define rte (omega-expr-idx r r1))
     (match-define (meaning c-idxs c-fwd c-comp) (run-expression c-expr r0 r))
     (define t-meaning (delay (run-expression t-expr r rte)))
     (define f-meaning (delay (run-expression f-expr rte r1)))
     (define t-idxs (delay (meaning-indexes (force t-meaning))))
     (define f-idxs (delay (meaning-indexes (force f-meaning))))
     (meaning
      (append c-idxs (list (if-indexes r t-idxs f-idxs)))
      (λ (γ bs)
        (define c (c-fwd γ bs))
        (define b (branches-ref bs r))
        (cond [(or (not (boolean? c)) (not (boolean-set-member? b c)))  (raise 'if-bad-branch)]
              [c     ((meaning-forward (force t-meaning)) γ bs)]
              [else  ((meaning-forward (force f-meaning)) γ bs)]))
      (λ (orig-domain bs)
        (define c (c-comp orig-domain bs))
        (define c-range (function-range c))
        
        (define t-domain (if (rect-member? c-range #t) (apply-preimage c 't) empty-set))
        (define f-domain (if (rect-member? c-range #f) (apply-preimage c 'f) empty-set))
        
        ;; t? = #t if it's possible to take the true branch
        ;; f? = #t if it's possible to take the false branch
        (define-values (t? f?)
          (let-values ([(bt? bf?)  (boolean-set->booleans (branches-ref bs r))])
            (values (and bt? (not (empty-set? t-domain)))
                    (and bf? (not (empty-set? f-domain))))))
        
        (define c-new-bs (function-new-branches c))
        
        (cond [(and t? f?)
               (define domain (rect-join t-domain f-domain))
               (cond [strict?
                      (let ([t  ((meaning-computation (force t-meaning)) t-domain bs)]
                            [f  ((meaning-computation (force f-meaning)) f-domain bs)])
                        (define t-range (function-range t))
                        (define f-range (function-range f))
                        (define range (rect-join t-range f-range))
                        (make-function domain c-new-bs range
                                       (λ (B)
                                         (rect-join (apply-preimage t (rect-intersect t-range B))
                                                    (apply-preimage f (rect-intersect f-range B))))))]
                     [else
                      (make-function domain c-new-bs universal-set (λ (B) domain))])]
              [t?
               (let ([t  ((meaning-computation (force t-meaning)) t-domain bs)])
                 (define t-range (function-range t))
                 (define new-bs (cons (cons r 't) (append c-new-bs (function-new-branches t))))
                 (make-function t-domain new-bs t-range (λ (B) (apply-preimage t B))))]
              [f?
               (let ([f  ((meaning-computation (force f-meaning)) f-domain bs)])
                 (define f-range (function-range f))
                 (define new-bs (cons (cons r 'f) (append c-new-bs (function-new-branches f))))
                 (make-function f-domain new-bs f-range (λ (B) (apply-preimage f B))))]
              [else
               (make-function empty-set c-new-bs empty-set (λ (B) empty-set))]))))))

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
                                empty/arr)))

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
                                empty/arr)))
|#
