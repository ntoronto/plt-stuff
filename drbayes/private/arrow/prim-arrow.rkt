#lang typed/racket/base

(require racket/match
         racket/flonum
         (only-in racket/math pi)
         math/distributions
         "../set.rkt"
         "arrow-common.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Primitive expressions

(define-type Prim-Forward-Fun (Value -> Value))
(define-type Prim-Preimage-Fun (Nonempty-Set -> Set))

(define-type Prim-Computation-Meaning (U Empty-Meaning prim-computation-meaning))
(define-type Prim-Computation (Nonempty-Set -> Prim-Computation-Meaning))

(struct: prim-computation-meaning ([K : Nonempty-Set]
                                   [preimage : Prim-Preimage-Fun])
  #:transparent)

(struct: prim-expression ([fun : (-> prim-expression-meaning)])
  #:transparent)

(struct: prim-expression-meaning ([forward : Prim-Forward-Fun]
                                  [computation : Prim-Computation])
  #:transparent)

;; ===================================================================================================
;; Convenience functions

(: run-prim-expression (prim-expression -> prim-expression-meaning))
(define (run-prim-expression e)
  ((prim-expression-fun e)))

(: run-prim-preimage (Prim-Preimage-Fun Set -> Set))
(define (run-prim-preimage pre K)
  (if (empty-set? K) empty-set (pre K)))

;; ===================================================================================================
;; Caching wrappers

(: prim-preimage (Set Set Prim-Preimage-Fun -> Prim-Preimage-Fun))
;; Wraps a Prim-Preimage-Fun with code that ensures the argument is a subset of the range; also
;; caches return values
(define ((prim-preimage Γ K pre) Ksub)
  (let ([Ksub  (set-intersect K Ksub)])
    (cond
      [(empty-set? Ksub)
       empty-set]
      [(not prim-cache-preimages?)
       (pre Ksub)]
      [(eq? Ksub K)
       (when prim-preimage-stats? (increment-cache-stat 'prim-preimage-hits))
       Γ]
      [else
       (when prim-preimage-stats?
         (increment-cache-stat 'prim-preimage-misses)
         (when (and (not (eq? Ksub K)) (equal? Ksub K))
           (increment-cache-stat 'prim-preimage-misses/bad-K)))
       (pre Ksub)])))

(: cached-prim-computation (Nonempty-Set Prim-Computation -> Prim-Computation))
(define (cached-prim-computation domain comp)
  (define: last-Γ : (U #f Nonempty-Set)  #f)
  (define: last-m : (U #f Prim-Computation-Meaning)  #f)
  (λ (Γ)
    (let ([Γ  (set-intersect Γ domain)])
      (define cached-m last-m)
      (cond
        [(empty-set? Γ)  empty-meaning]
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

(: bottom/fwd Prim-Forward-Fun)
(define (bottom/fwd γ) (raise (forward-fail "bottom")))

(: bottom/comp Prim-Computation)
(define (bottom/comp Γ) empty-meaning)

(define bottom/arr
  (prim-expression (λ () (prim-expression-meaning bottom/fwd bottom/comp))))

;; Identity function

(: id/fwd Prim-Forward-Fun)
(define (id/fwd γ) γ)

(: id/pre Prim-Preimage-Fun)
(define (id/pre K) K)

(: id/comp (-> Prim-Computation))
(define (id/comp)
  (cached-prim-computation
   universe
   (λ (Γ) (prim-computation-meaning Γ (prim-preimage Γ Γ id/pre)))))

(define id/arr
  (prim-expression (λ () (prim-expression-meaning id/fwd (id/comp)))))

;; Constant functions

(: c/fwd (Value -> Prim-Forward-Fun))
(define ((c/fwd x) γ) x)

(: c/pre (Nonempty-Set -> Prim-Preimage-Fun))
(define ((c/pre X) K) X)

(: c/comp (Nonempty-Set Nonempty-Set -> Prim-Computation))
(define (c/comp domain X)
  (cached-prim-computation
   domain
   (λ (Γ) (prim-computation-meaning X (prim-preimage Γ X (c/pre X))))))

(: c/arr (case-> (Value -> prim-expression)
                 (Value Nonempty-Set -> prim-expression)))
(define (c/arr x [domain universe])
  (define fwd (c/fwd x))
  (define X (value->singleton x))
  (prim-expression (λ () (prim-expression-meaning fwd (c/comp domain X)))))

;; ===================================================================================================
;; Arrow composition (reverse composition)

(: prim-rcompose/fwd (Prim-Forward-Fun Prim-Forward-Fun -> Prim-Forward-Fun))
(define ((prim-rcompose/fwd f-fwd g-fwd) γ)
  (let* ([kf  (f-fwd γ)]
         [kg  (g-fwd kf)])
    kg))

(: prim-rcompose/pre (Prim-Preimage-Fun Prim-Preimage-Fun -> Prim-Preimage-Fun))
(define ((prim-rcompose/pre f-pre g-pre) Kg)
  (let* ([Kf  (g-pre Kg)]
         [Γf  (run-prim-preimage f-pre Kf)])
    Γf))

(: prim-rcompose/comp (Prim-Computation Prim-Computation -> Prim-Computation))
(define (prim-rcompose/comp f-comp g-comp)
  (cached-prim-computation
   universe
   (λ (Γf)
     (define f-meaning (f-comp Γf))
     (cond
       [(empty-meaning? f-meaning)  empty-meaning]
       [else
        (match-define (prim-computation-meaning Γg f-pre) f-meaning)
        (define g-meaning (g-comp Γg))
        (cond
          [(empty-meaning? g-meaning)  empty-meaning]
          [else
           (match-define (prim-computation-meaning Kg g-pre) g-meaning)
           (define pre (prim-preimage Γf Kg (prim-rcompose/pre f-pre g-pre)))
           (prim-computation-meaning Kg pre)])]))))

(: prim-rcompose/arr (prim-expression prim-expression -> prim-expression))
(define (prim-rcompose/arr f-expr g-expr)
  (prim-expression
   (λ ()
     (match-define (prim-expression-meaning f-fwd f-comp) (run-prim-expression f-expr))
     (match-define (prim-expression-meaning g-fwd g-comp) (run-prim-expression g-expr))
     (prim-expression-meaning (prim-rcompose/fwd f-fwd g-fwd)
                              (prim-rcompose/comp f-comp g-comp)))))

;; ===================================================================================================
;; Pairing

(: prim-pair/fwd (Prim-Forward-Fun Prim-Forward-Fun -> Prim-Forward-Fun))
(define ((prim-pair/fwd fst-fwd snd-fwd) γ)
  (let ([k1  (fst-fwd γ)]
        [k2  (snd-fwd γ)])
    (cons k1 k2)))

(: prim-pair/pre (Prim-Preimage-Fun Prim-Preimage-Fun -> Prim-Preimage-Fun))
(define ((prim-pair/pre pre1 pre2) K)
  (let ([Γ1  (run-prim-preimage pre1 (set-pair-ref K 'fst))]
        [Γ2  (run-prim-preimage pre2 (set-pair-ref K 'snd))])
    (set-intersect Γ1 Γ2)))

(: prim-pair/comp (Prim-Computation Prim-Computation -> Prim-Computation))
(define (prim-pair/comp comp1 comp2)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define meaning1 (comp1 Γ))
     (define meaning2 (if (empty-meaning? meaning1) empty-meaning (comp2 Γ)))
     (cond
       [(empty-meaning? meaning2)  empty-meaning]
       [else
        (match-define (prim-computation-meaning K1 pre1) meaning1)
        (match-define (prim-computation-meaning K2 pre2) meaning2)
        (define K (set-pair K1 K2))
        (define pre (prim-preimage Γ K (prim-pair/pre pre1 pre2)))
        (prim-computation-meaning K pre)]))))

(: prim-pair/arr (prim-expression prim-expression -> prim-expression))
(define (prim-pair/arr expr1 expr2)
  (prim-expression
   (λ ()
     (match-define (prim-expression-meaning fwd1 comp1) (run-prim-expression expr1))
     (match-define (prim-expression-meaning fwd2 comp2) (run-prim-expression expr2))
     (prim-expression-meaning (prim-pair/fwd fwd1 fwd2)
                              (prim-pair/comp comp1 comp2)))))

;; ===================================================================================================
;; Pair ref

(: ref/fwd (Pair-Index -> Prim-Forward-Fun))
(define ((ref/fwd j) γ) (value-pair-ref γ j))

(: ref/pre (Nonempty-Set Pair-Index -> Prim-Preimage-Fun))
(define ((ref/pre Γ j) K) (set-pair-set Γ j K))

(: ref/comp (Pair-Index -> Prim-Computation))
(define (ref/comp j)
  (cached-prim-computation
   all-pairs
   (λ (Γ)
     (define K (set-pair-ref Γ j))
     (cond [(empty-set? K)  empty-meaning]
           [else  (define pre (prim-preimage Γ K (ref/pre Γ j)))
                  (prim-computation-meaning K pre)]))))

(: ref/arr (Pair-Index -> prim-expression))
(define (ref/arr j)
  (prim-expression (λ () (prim-expression-meaning (ref/fwd j) (ref/comp j)))))

;; ===================================================================================================
;; Strict conditional

(: prim-if/fwd (Prim-Forward-Fun Prim-Forward-Fun Prim-Forward-Fun -> Prim-Forward-Fun))
(define ((prim-if/fwd c-fwd t-fwd f-fwd) γ)
  (define kc (c-fwd γ))
  (cond [(eq? kc #t)  (t-fwd γ)]
        [(eq? kc #f)  (f-fwd γ)]
        [else  (raise-argument-error 'prim-if/fwd "Boolean" kc)]))

(: prim-if-one/pre (Nonempty-Set Prim-Preimage-Fun -> Prim-Preimage-Fun))
(define ((prim-if-one/pre orig-K pre) K)
  (run-prim-preimage pre (set-intersect orig-K K)))

(: prim-if-both/pre (Nonempty-Set Nonempty-Set Prim-Preimage-Fun Prim-Preimage-Fun
                                  -> Prim-Preimage-Fun))
(define ((prim-if-both/pre Kt Kf t-pre f-pre) K)
  (set-join (run-prim-preimage t-pre (set-intersect Kt K))
            (run-prim-preimage f-pre (set-intersect Kf K))))

(: prim-if/comp (Prim-Computation Prim-Computation Prim-Computation -> Prim-Computation))
(define (prim-if/comp c-comp t-comp f-comp)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define c-meaning (c-comp Γ))
     (cond
       [(empty-meaning? c-meaning)  empty-meaning]
       [else
        (match-define (prim-computation-meaning Kc c-pre) c-meaning)
        (define Γt (if (set-member? Kc #t) (c-pre trues) empty-set))
        (define Γf (if (set-member? Kc #f) (c-pre falses) empty-set))
        
        (define meaning
          (cond
            [(and (empty-set? Γt) (empty-set? Γf))  empty-meaning]
            [(empty-set? Γf)
             (define t-meaning (t-comp Γt))
             (cond [(empty-meaning? t-meaning)  empty-meaning]
                   [else  (match-define (prim-computation-meaning Kt t-pre) t-meaning)
                          (prim-computation-meaning Kt (prim-if-one/pre Kt t-pre))])]
            [(empty-set? Γt)
             (define f-meaning (f-comp Γf))
             (cond [(empty-meaning? f-meaning)  empty-meaning]
                   [else  (match-define (prim-computation-meaning Kf f-pre) f-meaning)
                          (prim-computation-meaning Kf (prim-if-one/pre Kf f-pre))])]
            [else
             (define t-meaning (t-comp Γt))
             (define f-meaning (f-comp Γf))
             (cond [(and (empty-meaning? t-meaning) (empty-meaning? f-meaning))  empty-meaning]
                   [(empty-meaning? f-meaning)
                    (match-define (prim-computation-meaning Kt t-pre) t-meaning)
                    (prim-computation-meaning Kt (prim-if-one/pre Kt t-pre))]
                   [(empty-meaning? t-meaning)
                    (match-define (prim-computation-meaning Kf f-pre) f-meaning)
                    (prim-computation-meaning Kf (prim-if-one/pre Kf f-pre))]
                   [else
                    (match-define (prim-computation-meaning Kt t-pre) t-meaning)
                    (match-define (prim-computation-meaning Kf f-pre) f-meaning)
                    (prim-computation-meaning (set-join Kt Kf)
                                              (prim-if-both/pre Kt Kf t-pre f-pre))])]))
        
        (cond [(empty-meaning? meaning)  empty-meaning]
              [else  (match-define (prim-computation-meaning K pre) meaning)
                     (prim-computation-meaning K (prim-preimage (pre universe) K pre))])]))))

(: prim-if/arr (prim-expression prim-expression prim-expression -> prim-expression))
(define (prim-if/arr c-expr t-expr f-expr)
  (prim-expression
   (λ ()
     (match-define (prim-expression-meaning c-fwd c-comp) (run-prim-expression c-expr))
     (match-define (prim-expression-meaning t-fwd t-comp) (run-prim-expression t-expr))
     (match-define (prim-expression-meaning f-fwd f-comp) (run-prim-expression f-expr))
     (prim-expression-meaning (prim-if/fwd c-fwd t-fwd f-fwd)
                              (prim-if/comp c-comp t-comp f-comp)))))

;; ===================================================================================================
;; Monotone R -> R functions

(: monotone/fwd (Symbol (Flonum -> Flonum) -> Prim-Forward-Fun))
(define ((monotone/fwd name f) γ)
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

(: monotone/comp (Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                           -> Prim-Computation))
(define (monotone/comp f-domain f-range f g fx?)
  (cached-prim-computation
   f-domain
   (λ (Γ)
     (define K (monotone-range (assert Γ interval?) f-range f fx?))
     (cond [(empty-set? K)  empty-meaning]
           [else  (define pre (prim-preimage Γ K (monotone/pre Γ g fx?)))
                  (prim-computation-meaning K pre)]))))

(: monotone/arr (Symbol Interval Interval (Flonum -> Flonum) (Flonum -> Flonum) Boolean
                        -> prim-expression))
(define (monotone/arr name f-domain f-range f g fx?)
  (prim-expression
   (λ () (prim-expression-meaning (monotone/fwd name f)
                                  (monotone/comp f-domain f-range f g fx?)))))

;; ===================================================================================================
;; Monotone R x R -> R functions

(: monotone2d/fwd (Symbol (Flonum Flonum -> Flonum) -> Prim-Forward-Fun))
(define ((monotone2d/fwd name f) γ)
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
                              -> Prim-Computation))
(define (monotone2d/comp f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (cached-prim-computation
   f-domain
   (λ (Γ)
     (define K (monotone2d-range Γ f-range f fx? fy?))
     (cond [(empty-set? K)  empty-meaning]
           [else  (define pre (prim-preimage Γ K (monotone2d/pre Γ g gz? gy? h hz? hx?)))
                  (prim-computation-meaning K pre)]))))

(: monotone2d/arr (Symbol Pair-Rect Interval
                          (Flonum Flonum -> Flonum) Boolean Boolean
                          (Flonum Flonum -> Flonum) Boolean Boolean
                          (Flonum Flonum -> Flonum) Boolean Boolean
                          -> prim-expression))
(define (monotone2d/arr name f-domain f-range f fx? fy? g gz? gy? h hz? hx?)
  (prim-expression
   (λ () (prim-expression-meaning (monotone2d/fwd name f)
                                  (monotone2d/comp f-domain f-range f fx? fy? g gz? gy? h hz? hx?)))))

;; ===================================================================================================
;; Predicates

(: predicate/fwd ((Value -> Boolean) -> Prim-Forward-Fun))
(define ((predicate/fwd pred?) γ) (pred? γ))

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

(: predicate/comp (Nonempty-Set Nonempty-Set -> Prim-Computation))
(define (predicate/comp true-set false-set)
  (cached-prim-computation
   (set-join true-set false-set)
   (λ (Γ)
     (let ([true-set   (set-intersect Γ true-set)]
           [false-set  (set-intersect Γ false-set)])
       (define K (predicate-range true-set false-set))
       (cond [(empty-set? K)  empty-meaning]
             [else  (define pre (prim-preimage Γ K (predicate/pre true-set false-set)))
                    (prim-computation-meaning K pre)])))))

(: predicate/arr ((Value -> Boolean) Nonempty-Set Nonempty-Set -> prim-expression))
(define (predicate/arr pred? true-set false-set)
  (prim-expression
   (λ () (prim-expression-meaning (predicate/fwd pred?)
                                  (predicate/comp true-set false-set)))))

;; ===================================================================================================
;; Tagged values

(: tag?/arr (Set-Tag -> prim-expression))
(define (tag?/arr tag)
  (predicate/arr (λ: ([γ : Value]) (eq? tag (value-tag γ)))
                 (bot-set tag universe)
                 (top-set tag empty-set)))

;; ---------------------------------------------------------------------------------------------------

(: tag/fwd (Set-Tag -> Prim-Forward-Fun))
(define ((tag/fwd tag) γ) (tagged tag γ))

(: tag/pre (Set-Tag -> Prim-Preimage-Fun))
(define ((tag/pre tag) K) (set-untag K tag))

(: tag/comp (Set-Tag -> Prim-Computation))
(define (tag/comp tag)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define K (set-tag Γ tag))
     (define pre (prim-preimage Γ K (tag/pre tag)))
     (prim-computation-meaning K pre))))

(: tag/arr (Set-Tag -> prim-expression))
(define (tag/arr tag)
  (prim-expression (λ () (prim-expression-meaning (tag/fwd tag) (tag/comp tag)))))

;; ---------------------------------------------------------------------------------------------------

(: untag/fwd (Set-Tag -> Prim-Forward-Fun))
(define ((untag/fwd tag) γ)
  (if (and (tagged? γ) (eq? tag (get-tag γ)))
      (get-val γ)
      (raise (forward-fail (format "expected ~e; given ~e" tag γ)))))

(: untag/comp (Set-Tag -> Prim-Computation))
(define (untag/comp tag)
  (cached-prim-computation
   universe
   (λ (Γ)
     (define K (set-untag Γ tag))
     (cond [(empty-set? K)  empty-meaning]
           [else  (define pre (prim-preimage Γ K (λ (B) (set-tag B tag))))
                  (prim-computation-meaning K pre)]))))

(: untag/arr (Set-Tag -> prim-expression))
(define (untag/arr tag)
  (prim-expression (λ () (prim-expression-meaning (untag/fwd tag) (untag/comp tag)))))

;; ===================================================================================================
;; Data type predicates

(define real?/arr (predicate/arr flonum? real-interval (top-rect real-tag empty-set)))
(define null?/arr (predicate/arr null? null-rect (top-rect null-tag empty-set)))
(define pair?/arr (predicate/arr pair? all-pairs (top-rect pair-tag empty-set)))
(define boolean?/arr (predicate/arr boolean? booleans (top-rect boolean-tag empty-set)))

;; ===================================================================================================
;; Monotone R -> R functions

(: scale/arr (Flonum -> prim-expression))
(define (scale/arr y)
  (cond [(fl= y 0.0)  (c/arr 0.0)]
        [else  (monotone/arr 'scale/arr real-interval real-interval
                             (λ: ([x : Flonum]) (fl* x y))
                             (λ: ([z : Flonum]) (fl/ z y))
                             (y . fl> . 0.0))]))

(: translate/arr (Flonum -> prim-expression))
(define (translate/arr y)
  (monotone/arr 'translate/arr real-interval real-interval
                (λ: ([x : Flonum]) (fl+ x y))
                (λ: ([z : Flonum]) (fl- z y))
                #t))

(: flneg (Flonum -> Flonum))
(define (flneg x) (fl* -1.0 x))

(: flsqr (Flonum -> Flonum))
(define (flsqr x) (fl* x x))

(: flrecip (Flonum -> Flonum))
(define (flrecip x) (fl/ 1.0 x))

(: flneg-sqrt (Flonum -> Flonum))
(define (flneg-sqrt x) (- (flsqrt x)))

(define neg/arr (monotone/arr 'neg/arr real-interval real-interval flneg flneg #f))
(define exp/arr (monotone/arr 'exp/arr real-interval nonnegative-interval flexp fllog #t))
(define log/arr (monotone/arr 'log/arr nonnegative-interval real-interval fllog flexp #t))
(define sqrt/arr (monotone/arr 'sqrt/arr nonnegative-interval nonnegative-interval flsqrt flsqr #t))

(define asin/arr
  (monotone/arr 'asin/arr
                (Interval -1.0 1.0 #t #t)
                (Interval (/ pi -2.0) (/ pi 2.0) #t #t)
                flasin flsin #t))

(define acos/arr
  (monotone/arr 'acos/arr
                (Interval -1.0 1.0 #t #t)
                (Interval 0.0 pi #t #t)
                flacos flcos #f))

(define mono-sin/arr
  (monotone/arr 'mono-sin/arr
                (Interval (/ pi -2.0) (/ pi 2.0) #t #t)
                (Interval -1.0 1.0 #t #t)
                flsin flasin #t))

(define mono-cos/arr
  (monotone/arr 'mono-cos/arr
                (Interval 0.0 pi #t #t)
                (Interval -1.0 1.0 #t #t)
                flcos flacos #f))

(define pos-recip/arr
  (monotone/arr 'recip/arr positive-interval positive-interval flrecip flrecip #t))
(define neg-recip/arr
  (monotone/arr 'recip/arr negative-interval negative-interval flrecip flrecip #f))

(define pos-sqr/arr
  (monotone/arr 'sqr/arr nonnegative-interval nonnegative-interval flsqr flsqrt #t))
(define neg-sqr/arr
  (monotone/arr 'sqr/arr negative-interval positive-interval flsqr flneg-sqrt #f))

(: inverse-cdf/arr (Symbol Interval (Flonum -> Flonum) (Flonum -> Flonum) -> prim-expression))
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

(define cauchy/arr (inverse-cdf/arr 'cauchy/arr real-interval cauchy-inv-cdf cauchy-cdf))
(define normal/arr (inverse-cdf/arr 'normal/arr real-interval normal-inv-cdf normal-cdf))

;; ===================================================================================================
;; Monotone R x R -> R functions

(define real-pair (pair-rect real-interval real-interval))

(define +/arr
  (monotone2d/arr '+/arr real-pair real-interval
                  fl+ #t #t
                  fl- #t #f
                  fl- #t #f))

(: neg-fl- (Flonum Flonum -> Flonum))
(define (neg-fl- z x) (fl- x z))

(define -/arr
  (monotone2d/arr '-/arr real-pair real-interval
                  fl- #t #f
                  fl+ #t #t
                  neg-fl- #f #t))

(define pos-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-interval nonnegative-interval) nonnegative-interval
                  fl* #t #t
                  fl/ #t #f
                  fl/ #t #f))

(define pos-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect nonnegative-interval negative-interval) nonpositive-interval
                  fl* #f #t
                  fl/ #f #t
                  fl/ #t #t))

(define neg-pos-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-interval nonnegative-interval) nonpositive-interval
                  fl* #t #f
                  fl/ #t #t
                  fl/ #f #t))

(define neg-neg-mul/arr
  (monotone2d/arr '*/arr (pair-rect negative-interval negative-interval) positive-interval
                  fl* #f #f
                  fl/ #f #f
                  fl/ #f #f))

(: recip-fl/ (Flonum Flonum -> Flonum))
(define (recip-fl/ z x) (fl/ x z))

(define pos-pos-div/arr
  (monotone2d/arr '//arr (pair-rect positive-interval positive-interval) positive-interval
                  fl/ #t #f
                  fl* #t #t
                  recip-fl/ #f #t))

(define pos-neg-div/arr
  (monotone2d/arr '//arr (pair-rect positive-interval negative-interval) negative-interval
                  fl/ #f #f
                  fl* #f #f
                  recip-fl/ #f #f))

(define neg-pos-div/arr
  (monotone2d/arr '//arr (pair-rect negative-interval positive-interval) negative-interval
                  fl/ #t #t
                  fl* #t #f
                  recip-fl/ #t #f))

(define neg-neg-div/arr
  (monotone2d/arr '//arr (pair-rect negative-interval negative-interval) positive-interval
                  fl/ #f #t
                  fl* #f #t
                  recip-fl/ #t #t))

;; ===================================================================================================
;; Real predicates

(: real-predicate/arr (Symbol (Flonum -> Boolean) Interval Interval -> prim-expression))
(define (real-predicate/arr name p? true-ivl false-ivl)
  (predicate/arr (λ: ([γ : Value])
                   (if (flonum? γ) (p? γ) (raise-argument-error name "Flonum" γ)))
                 true-ivl false-ivl))

(define negative?/arr
  (real-predicate/arr 'negative?/arr (λ: ([x : Flonum]) (x . fl< . 0.0))
                      negative-interval nonnegative-interval))

(define positive?/arr
  (real-predicate/arr 'positive?/arr (λ: ([x : Flonum]) (x . fl> . 0.0))
                      positive-interval nonpositive-interval))

(define nonpositive?/arr
  (real-predicate/arr 'nonpositive?/arr (λ: ([x : Flonum]) (x . fl<= . 0.0))
                      nonpositive-interval positive-interval))

(define nonnegative?/arr
  (real-predicate/arr 'nonnegative?/arr (λ: ([x : Flonum]) (x . fl>= . 0.0))
                      nonnegative-interval negative-interval))

(define lt/arr (prim-rcompose/arr -/arr negative?/arr))
(define gt/arr (prim-rcompose/arr -/arr positive?/arr))
(define lte/arr (prim-rcompose/arr -/arr nonpositive?/arr))
(define gte/arr (prim-rcompose/arr -/arr nonnegative?/arr))

;; ===================================================================================================
;; Non-monotone functions

;; Square
(define sqr/arr
  (prim-if/arr negative?/arr neg-sqr/arr pos-sqr/arr))

;; Reciprocal
(define recip/arr
  (prim-if/arr positive?/arr
               pos-recip/arr
               (prim-if/arr negative?/arr
                            neg-recip/arr
                            bottom/arr)))

#|
Sine and cosine arrow functions are direct translations of the following Racket functions:

(define (partial-cos x)
  (if (negative? x)
      (mono-cos (- x))
      (mono-cos x)))

(define (partial-pos-sin x)
  (if (nonpositive? (+ x (* -0.5 pi)))
      (mono-sin x)
      (let ([x  (+ x (- pi))])
        (if (nonpositive? x)
            (mono-sin (- x))
            (error 'bottom)))))

(define (partial-sin x)
  (if (negative? x)
      (- (partial-pos-sin (- x)))
      (partial-pos-sin x)))

|#

;; Cosine restricted to [-π,π]
(define partial-cos/arr
  (prim-if/arr negative?/arr
               (prim-rcompose/arr neg/arr mono-cos/arr)
               mono-cos/arr))

;; Sine restricted to [-π/2,π]
(define partial-pos-sin/arr
  (prim-if/arr (prim-rcompose/arr (translate/arr (* -0.5 pi)) nonpositive?/arr)
               mono-sin/arr
               (prim-rcompose/arr
                (translate/arr (- pi))
                (prim-if/arr nonpositive?/arr
                             (prim-rcompose/arr neg/arr mono-sin/arr)
                             bottom/arr))))

;; Sine restricted to [-π,π]
(define partial-sin/arr
  (prim-if/arr negative?/arr
               (prim-rcompose/arr (prim-rcompose/arr neg/arr partial-pos-sin/arr) neg/arr)
               partial-pos-sin/arr))

;; Multiplication
(define */arr
  (prim-if/arr (prim-rcompose/arr (ref/arr 'fst) positive?/arr)
               (prim-if/arr (prim-rcompose/arr (ref/arr 'snd) positive?/arr)
                            pos-pos-mul/arr
                            (prim-if/arr (prim-rcompose/arr (ref/arr 'snd) negative?/arr)
                                         pos-neg-mul/arr
                                         (c/arr 0.0 real-pair)))
               (prim-if/arr (prim-rcompose/arr (ref/arr 'fst) negative?/arr)
                            (prim-if/arr (prim-rcompose/arr (ref/arr 'snd) positive?/arr)
                                         neg-pos-mul/arr
                                         (prim-if/arr (prim-rcompose/arr (ref/arr 'snd) negative?/arr)
                                                      neg-neg-mul/arr
                                                      (c/arr 0.0 real-pair)))
                            (c/arr 0.0 real-pair))))

;; Division
(define //arr
  (prim-if/arr (prim-rcompose/arr (ref/arr 'snd) positive?/arr)
               (prim-if/arr (prim-rcompose/arr (ref/arr 'fst) positive?/arr)
                            pos-pos-div/arr
                            (prim-if/arr (prim-rcompose/arr (ref/arr 'fst) negative?/arr)
                                         neg-pos-div/arr
                                         (c/arr 0.0)))
               (prim-if/arr (prim-rcompose/arr (ref/arr 'snd) negative?/arr)
                            (prim-if/arr (prim-rcompose/arr (ref/arr 'fst) positive?/arr)
                                         pos-neg-div/arr
                                         (prim-if/arr (prim-rcompose/arr (ref/arr 'fst) negative?/arr)
                                                      neg-neg-div/arr
                                                      (c/arr 0.0)))
                            bottom/arr)))
