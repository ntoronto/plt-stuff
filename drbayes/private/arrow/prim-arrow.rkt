#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/promise
         (only-in racket/math pi)
         math/flonum
         math/distributions
         "../set.rkt"
         "../untyped-utils.rkt"
         "arrow-common.rkt"
         "directed-rounding-flops.rkt")

(provide (all-defined-out))

(define check-prim-preimage-arguments? #t)

;; ===================================================================================================
;; Primitive expressions

(define-type Prim-Forward-Fun (Value -> Maybe-Value))
(define-type Prim-Preimage-Fun (Nonempty-Set Nonempty-Set -> Set))
(define-type Prim-Image-Fun (Nonempty-Set Nonempty-Set -> Set))

(define-type Prim-Preimage (U Empty-Preimage nonempty-prim-preimage))

(struct: nonempty-prim-preimage ([domain : Nonempty-Set]
                                 [range : Nonempty-Set]
                                 [fun : Prim-Preimage-Fun])
  #:transparent)

(define-type Prim-Computation (Nonempty-Set -> Prim-Preimage))

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

(: run-prim-forward-fun (Prim-Forward-Fun Maybe-Value -> Maybe-Value))
(define (run-prim-forward-fun f γ)
  (cond [(bottom? γ)  γ]
        [else  (f γ)]))

(: run-prim-preimage-fun (Prim-Preimage-Fun Set Set -> Set))
(define (run-prim-preimage-fun f Γ K)
  (cond [(or (empty-set? Γ) (empty-set? K))  empty-set]
        [else  (f Γ K)]))

(: run-prim-preimage (Prim-Preimage Set Set -> Set))
(define (run-prim-preimage f-pre Γ K)
  (cond [(or (empty-preimage? f-pre) (empty-set? Γ) (empty-set? K))  empty-set]
        [else
         (match-define (prim-preimage Γf Kf f) f-pre)
         (cond [(and check-prim-preimage-arguments? (not (set-subseteq? Γ Γf)))
                (raise-argument-error 'run-prim-preimage (format "subset of ~e" Γf) 1 f-pre Γ K)]
               [(and check-prim-preimage-arguments? (not (set-subseteq? K Kf)))
                (raise-argument-error 'run-prim-preimage (format "subset of ~e" Kf) 2 f-pre Γ K)]
               [else
                (f Γ K)])]))

(: run-prim-image-fun (Prim-Image-Fun Set Set -> Set))
(define run-prim-image-fun run-prim-preimage-fun)

(: run-prim-computation (Prim-Computation Set -> Prim-Preimage))
(define (run-prim-computation comp Γ)
  (cond [(empty-set? Γ)  empty-preimage]
        [else  (comp Γ)]))

(: make-prim-preimage (Set Set Prim-Preimage-Fun -> Prim-Preimage))
(define (make-prim-preimage Γ K f)
  (cond [(or (empty-set? Γ) (empty-set? K))  empty-preimage]
        [else  (nonempty-prim-preimage Γ K f)]))

(: empty-prim-preimage-fun Prim-Preimage-Fun)
(define (empty-prim-preimage-fun Γ K) empty-set)

(define-match-expander prim-preimage
  (λ (stx)
    (syntax-case stx ()
      [(_ Γ K f)  (syntax/loc stx
                    (or (nonempty-prim-preimage Γ K f)
                        (and (? empty-preimage?)
                             (app (λ (_) empty-set) Γ)
                             (app (λ (_) empty-set) K)
                             (app (λ (_) empty-prim-preimage-fun) f))
                        (and (app (λ: ([_ : Prim-Preimage]) empty-set) Γ)
                             (app (λ (_) empty-set) K)
                             (app (λ (_) empty-prim-preimage-fun) f))))]))
  (make-head-form #'make-prim-preimage))

;; ===================================================================================================
;; Caching wrappers
#|
(: prim-preimage (Nonempty-Set Nonempty-Set Prim-Preimage-Fun -> Prim-Preimage-Fun))
;; Wraps a Prim-Preimage-Fun with code that ensures the argument is a subset of the range; also
;; caches return values
(define (prim-preimage Γ K pre)
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
|#

;; ===================================================================================================
;; Basic primitives

;; Bottom function: has empty range

(: bottom/fwd Prim-Forward-Fun)
(define (bottom/fwd γ) (bottom (delay "fail")))

(: bottom/comp Prim-Computation)
(define (bottom/comp Γ) empty-preimage)

(define bottom/arr
  (prim-expression (λ () (prim-expression-meaning bottom/fwd bottom/comp))))

;; Identity function

(: id/fwd Prim-Forward-Fun)
(define (id/fwd γ) γ)

(: id/pre Prim-Preimage-Fun)
(define (id/pre Γ K) (set-intersect Γ K))

(: id/comp Prim-Computation)
(define (id/comp Γ) (nonempty-prim-preimage Γ Γ id/pre))

(define id/arr
  (prim-expression (λ () (prim-expression-meaning id/fwd id/comp))))

;; Constant functions

(: c/fwd (Value -> Prim-Forward-Fun))
(define ((c/fwd x) γ) (if (bottom? γ) γ x))

(: c/pre Prim-Preimage-Fun)
(define (c/pre Γ K) (if (empty-set? K) K Γ))

(: c/comp (Nonempty-Set Nonempty-Set -> Prim-Computation))
(define ((c/comp domain X) Γ)
  (prim-preimage (set-intersect Γ domain) X c/pre))

(: c/arr (case-> (Value -> prim-expression)
                 (Value Nonempty-Set -> prim-expression)))
(define (c/arr x [domain universe])
  (define fwd (c/fwd x))
  (define X (value->singleton x))
  (prim-expression (λ () (prim-expression-meaning fwd (c/comp domain X)))))

;; Late booleans, used only for testing
;; Used as an if condition, they forcing refinement sampling to always choose a branch

(: late-boolean/fwd (Flonum -> Prim-Forward-Fun))
(define ((late-boolean/fwd p) γ) (if ((random) . < . p) #t #f))

(: late-boolean/comp Prim-Computation)
(define (late-boolean/comp Γ)
  (prim-preimage Γ bools (λ (Γ K) Γ)))

(: late-boolean/arr (Flonum -> prim-expression))
(define (late-boolean/arr p)
  (prim-expression (λ () (prim-expression-meaning (late-boolean/fwd p) late-boolean/comp))))

;; ===================================================================================================
;; Arrow composition (reverse composition)

(: prim-rcompose/fwd (Prim-Forward-Fun Prim-Forward-Fun -> Prim-Forward-Fun))
(define ((prim-rcompose/fwd f-fwd g-fwd) γ)
  (run-prim-forward-fun g-fwd (run-prim-forward-fun f-fwd γ)))

(: prim-rcompose/pre (Prim-Preimage Prim-Preimage Set -> Prim-Preimage-Fun))
(define ((prim-rcompose/pre f-pre g-pre Γg) Γ K)
  (run-prim-preimage f-pre Γ (run-prim-preimage g-pre Γg K)))

(: prim-rcompose/comp (Prim-Computation Prim-Computation -> Prim-Computation))
(define ((prim-rcompose/comp f-comp g-comp) Γ)
  (match-define (and f-pre (prim-preimage Γf Kf f)) (f-comp Γ))
  (match-define (and g-pre (prim-preimage Γg Kg g)) (run-prim-computation g-comp Kf))
  (prim-preimage Γf Kg (prim-rcompose/pre f-pre g-pre Γg)))

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
  (define k1 (fst-fwd γ))
  (cond [(bottom? k1)  k1]
        [else  (define k2 (snd-fwd γ))
               (cond [(bottom? k2)  k2]
                     [else  (cons k1 k2)])]))

(: prim-pair/pre (Prim-Preimage Prim-Preimage -> Prim-Preimage-Fun))
(define ((prim-pair/pre pre1 pre2) Γ K)
  (define K1 (set-pair-ref K 'fst))
  (define K2 (set-pair-ref K 'snd))
  (run-prim-preimage pre1 (run-prim-preimage pre2 Γ K2) K1))

(: prim-pair/comp (Prim-Computation Prim-Computation -> Prim-Computation))
(define ((prim-pair/comp comp1 comp2) Γ)
  (match-define (and pre1 (prim-preimage Γ1 K1 f1)) (comp1 Γ))
  (match-define (and pre2 (prim-preimage Γ2 K2 f2)) (run-prim-computation comp2 Γ1))
  (prim-preimage Γ2 (set-pair K1 K2) (prim-pair/pre pre1 pre2)))

(: prim-pair/arr (prim-expression prim-expression -> prim-expression))
(define (prim-pair/arr expr1 expr2)
  (prim-expression
   (λ ()
     (match-define (prim-expression-meaning fwd1 comp1) (run-prim-expression expr1))
     (match-define (prim-expression-meaning fwd2 comp2) (run-prim-expression expr2))
     (prim-expression-meaning (prim-pair/fwd fwd1 fwd2)
                              (prim-pair/comp comp1 comp2)))))

;; ===================================================================================================
;; Strict conditional

(: prim-if/fwd (Prim-Forward-Fun Prim-Forward-Fun Prim-Forward-Fun -> Prim-Forward-Fun))
(define ((prim-if/fwd c-fwd t-fwd f-fwd) γ)
  (define kc (c-fwd γ))
  (cond [(bottom? kc)  kc]
        [(eq? kc #t)  (t-fwd γ)]
        [(eq? kc #f)  (f-fwd γ)]
        [else  (bottom (delay (format "prim-if: expected Boolean condition; given ~e" kc)))]))

(: prim-if/pre (Prim-Preimage Prim-Preimage -> Prim-Preimage))
(define (prim-if/pre t-pre f-pre)
  (match-define (prim-preimage Γt Kt t) t-pre)
  (match-define (prim-preimage Γf Kf f) f-pre)
  (prim-preimage
   (set-join Γt Γf)
   (set-join Kt Kf)
   (λ (Γ K)
     (set-join (run-prim-preimage t-pre (set-intersect Γ Γt) (set-intersect K Kt))
               (run-prim-preimage f-pre (set-intersect Γ Γf) (set-intersect K Kf))))))

(: prim-if/comp (Prim-Computation Prim-Computation Prim-Computation -> Prim-Computation))
(define ((prim-if/comp c-comp t-comp f-comp) Γ)
  (match-define (and c-pre (prim-preimage Γc Kc c)) (c-comp Γ))
  (define Γt (if (set-member? Kc #t) (run-prim-preimage c-pre Γc trues)  empty-set))
  (define Γf (if (set-member? Kc #f) (run-prim-preimage c-pre Γc falses) empty-set))
  (prim-if/pre (run-prim-computation t-comp Γt)
               (run-prim-computation f-comp Γf)))

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
;; Pair ref

(: ref/fwd (Pair-Index -> Prim-Forward-Fun))
(define ((ref/fwd j) γ)
  (if (bottom? γ) γ (value-pair-ref γ j)))

(: ref/pre (Pair-Index -> Prim-Preimage-Fun))
(define ((ref/pre j) Γ K)
  (set-pair-restrict Γ j K))

(: ref/comp (Pair-Index -> Prim-Computation))
(define ((ref/comp j) Γ)
  (prim-preimage Γ (set-pair-ref Γ j) (ref/pre j)))

(: ref/arr (Pair-Index -> prim-expression))
(define (ref/arr j)
  (prim-expression (λ () (prim-expression-meaning (ref/fwd j) (ref/comp j)))))

;; ===================================================================================================
;; Monotone R -> R functions

(: monotone/fwd (Symbol Nonempty-Real-Set Nonempty-Real-Set (Flonum -> Flonum) -> Prim-Forward-Fun))
(define ((monotone/fwd name f-domain f-range f) γ)
  (cond [(or (not (flonum? γ)) (not (real-set-member? f-domain γ)))
         (bottom (delay (format "~a: expected argument in ~a; given ~e" name f-domain γ)))]
        [else
         (define k (f γ))
         (cond [(not (real-set-member? f-range k))
                (bottom (delay (format "~a: expected result in ~a; produced ~e" name f-range k)))]
               [else  k])]))

(: monotone-apply (Boolean (Flonum -> Flonum) (Flonum -> Flonum) Nonempty-Interval -> Interval))
(define (monotone-apply inc? f/rndd f/rndu X)
  (define-values (a b a? b?) (interval-fields X))
  (cond [inc?  (interval (f/rndd a) (f/rndu b) a? b?)]
        [else  (interval (f/rndd b) (f/rndu a) b? a?)]))

(: monotone/img (Boolean (Flonum -> Flonum) (Flonum -> Flonum) -> Prim-Image-Fun))
(define ((monotone/img inc? f/rndd f/rndu) Γ K)
  (define Kf (real-set-map (λ (Γ) (monotone-apply inc? f/rndd f/rndu Γ))
                           (set-take-reals Γ)))
  (cond [(empty-real-set? Kf)  empty-set]
        [else  (set-intersect K Kf)]))

(: monotone/pre (Boolean (Flonum -> Flonum) (Flonum -> Flonum) -> Prim-Preimage-Fun))
(define ((monotone/pre inc? g/rndd g/rndu) Γ K)
  (define Γf (real-set-map (λ (K) (monotone-apply inc? g/rndd g/rndu K))
                           (set-take-reals K)))
  (cond [(empty-real-set? Γf)  empty-set]
        [else  (set-intersect Γ Γf)]))

(: monotone/comp (Nonempty-Real-Set Nonempty-Real-Set Boolean
                                    (Flonum -> Flonum)
                                    (Flonum -> Flonum) (Flonum -> Flonum)
                                    (Flonum -> Flonum) (Flonum -> Flonum)
                                    -> Prim-Computation))
(define (monotone/comp Γf Kf inc? f f/rndd f/rndu g/rndd g/rndu)
  (define img (monotone/img inc? f/rndd f/rndu))
  (define pre (monotone/pre inc? g/rndd g/rndu))
  (λ (Γ) (let ([Γ  (set-intersect Γ Γf)])
           (prim-preimage Γ (run-prim-image-fun img Γ Kf) pre))))

(: monotone/arr (Symbol Nonempty-Real-Set Nonempty-Real-Set Boolean
                        (Flonum -> Flonum)
                        (Flonum -> Flonum) (Flonum -> Flonum)
                        (Flonum -> Flonum) (Flonum -> Flonum)
                        -> prim-expression))
(define (monotone/arr name f-domain f-range inc? f f/rndd f/rndu g/rndd g/rndu)
  (prim-expression
   (λ () (prim-expression-meaning
          (monotone/fwd name f-domain f-range f)
          (monotone/comp f-domain f-range inc? f f/rndd f/rndu g/rndd g/rndu)))))

;; ===================================================================================================
;; Monotone R x R -> R functions

(: monotone2d/fwd (Symbol Bot-Basic Nonempty-Real-Set (Flonum Flonum -> Flonum) -> Prim-Forward-Fun))
(define ((monotone2d/fwd name Γf Kf f) γ)
  (cond [(or (not (pair? γ)) (not (set-member? Γf γ)))
         (bottom (delay (format "~a: expected argument in ~a; given ~e" name Γf γ)))]
        [else
         (match-define (cons (? flonum? x) (? flonum? y)) γ)
         (define k (f x y))
         (cond [(not (real-set-member? Kf k))
                (bottom (delay (format "~a: expected result in ~a; produced ~e" name Kf k)))]
               [else  k])]))

(: monotone2d-apply (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                             Nonempty-Interval Nonempty-Interval -> Interval))
(define (monotone2d-apply xinc? yinc? f/rndd f/rndu X Y)
  (let-values ([(xa xb xa? xb?)  (let-values ([(xa xb xa? xb?)  (interval-fields X)])
                                   (cond [xinc?  (values xa xb xa? xb?)]
                                         [else   (values xb xa xb? xa?)]))]
               [(ya yb ya? yb?)  (let-values ([(ya yb ya? yb?)  (interval-fields Y)])
                                   (cond [yinc?  (values ya yb ya? yb?)]
                                         [else   (values yb ya yb? ya?)]))])
    (interval (f/rndd xa ya) (f/rndu xb yb) (and xa? ya?) (and xb? yb?))))

(: monotone2d/img (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                           -> Prim-Image-Fun))
(define ((monotone2d/img xinc? yinc? f/rndd f/rndu) Γ K)
  (let* ([Γx  (set-take-reals (set-pair-ref Γ 'fst))]
         [Γy  (set-take-reals (set-pair-ref Γ 'snd))])
    (define Kf
      (real-set-map
       (λ (Γx) (real-set-map (λ (Γy) (monotone2d-apply xinc? yinc? f/rndd f/rndu Γx Γy)) Γy))
       Γx))
    (set-intersect K (bot-basic Kf))))

(: monotone2d/pre (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                           Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                           -> Prim-Preimage-Fun))
(define ((monotone2d/pre gz? gy? g/rndd g/rndu hz? hx? h/rndd h/rndu) Γ K)
  (let* ([K  (set-take-reals K)]
         [Γx  (set-take-reals (set-pair-ref Γ 'fst))]
         [Γy  (set-take-reals (set-pair-ref Γ 'snd))])
    (define X
      (real-set-map
       (λ (K) (real-set-map
               (λ (Γy) (monotone2d-apply gz? gy? g/rndd g/rndu K Γy))
               Γy))
       K))
    (define Y
      (real-set-map
       (λ (K) (real-set-map
               (λ (Γx) (monotone2d-apply hz? hx? h/rndd h/rndu K Γx))
               Γx))
       K))
    (set-intersect Γ (set-pair (bot-basic X) (bot-basic Y)))))

(: monotone2d/comp (Bot-Basic Nonempty-Real-Set
                              Boolean Boolean (Flonum Flonum -> Flonum)
                              (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                              Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                              Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                              -> Prim-Computation))
(define (monotone2d/comp Γf Kf
                         fx? fy? f f/rndd f/rndu
                         gz? gy? g/rndd g/rndu
                         hz? hx? h/rndd h/rndu)
  (define img (monotone2d/img fx? fy? f/rndd f/rndu))
  (define pre (monotone2d/pre gz? gy? g/rndd g/rndu hz? hx? h/rndd h/rndu))
  (λ (Γ) (let ([Γ  (set-intersect Γ Γf)])
           (prim-preimage Γ (run-prim-image-fun img Γ (bot-basic Kf)) pre))))

(: monotone2d/arr (Symbol Nonempty-Real-Set Nonempty-Real-Set Nonempty-Real-Set
                          Boolean Boolean (Flonum Flonum -> Flonum)
                          (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          -> prim-expression))
(define (monotone2d/arr name Γf1 Γf2 Kf
                        fx? fy? f f/rndd f/rndu
                        gz? gy? g/rndd g/rndu
                        hz? hx? h/rndd h/rndu)
  (define Γf (set-pair (bot-basic Γf1) (bot-basic Γf2)))
  (prim-expression
   (λ () (prim-expression-meaning (monotone2d/fwd name Γf Kf f)
                                  (monotone2d/comp Γf Kf
                                                   fx? fy? f f/rndd f/rndu
                                                   gz? gy? g/rndd g/rndu
                                                   hz? hx? h/rndd h/rndu)))))

;; ===================================================================================================
;; Predicates

(: predicate/fwd ((Value -> Boolean) -> Prim-Forward-Fun))
(define ((predicate/fwd pred?) γ) (pred? γ))

(: predicate/pre (Set Set -> Prim-Preimage-Fun))
(define ((predicate/pre Γt Γf) Γ K)
  (let ([Γt  (if (set-member? K #t) (set-intersect Γ Γt) empty-set)]
        [Γf  (if (set-member? K #f) (set-intersect Γ Γf) empty-set)])
    (set-join Γt Γf)))

(: predicate/comp (Nonempty-Set Nonempty-Set -> Prim-Computation))
(define ((predicate/comp Γt Γf) Γ)
  (let ([Γt  (set-intersect Γ Γt)]
        [Γf  (set-intersect Γ Γf)])
    (define Γ (set-join Γt Γf))
    (define K (bot-basic (booleans->bool-set (not (empty-set? Γt)) (not (empty-set? Γf)))))
    (prim-preimage Γ K (predicate/pre Γt Γf))))

(: predicate/arr ((Value -> Boolean) Nonempty-Set Nonempty-Set -> prim-expression))
(define (predicate/arr pred? Γt Γf)
  (prim-expression
   (λ () (prim-expression-meaning (predicate/fwd pred?)
                                  (predicate/comp Γt Γf)))))

;; ===================================================================================================
;; Tagged values

(: tag?/arr (Tag -> prim-expression))
(define (tag?/arr tag)
  (predicate/arr (λ: ([γ : Value])
                   (and (tagged-value? γ) (eq? tag (tagged-value-tag γ))))
                 (bot-tagged tag universe)
                 (top-tagged tag empty-set)))

;; ---------------------------------------------------------------------------------------------------

(: tag/fwd (Tag -> Prim-Forward-Fun))
(define ((tag/fwd tag) γ) (tagged-value tag γ))

(: tag/pre (Tag -> Prim-Preimage-Fun))
(define ((tag/pre tag) Γ K) (set-intersect Γ (set-untag K tag)))

(: tag/comp (Tag -> Prim-Computation))
(define ((tag/comp tag) Γ)
  (define K (set-tag Γ tag))
  (prim-preimage Γ K (tag/pre tag)))

(: tag/arr (Tag -> prim-expression))
(define (tag/arr tag)
  (prim-expression (λ () (prim-expression-meaning (tag/fwd tag) (tag/comp tag)))))

;; ---------------------------------------------------------------------------------------------------

(: untag/fwd (Tag -> Prim-Forward-Fun))
(define ((untag/fwd tag) γ)
  (if (and (tagged-value? γ) (eq? tag (tagged-value-tag γ)))
      (tagged-value-value γ)
      (bottom (delay (format "expected ~e; given ~e" tag γ)))))

(: untag/pre (Tag -> Prim-Preimage-Fun))
(define ((untag/pre tag) Γ K) (set-intersect Γ (set-tag K tag)))

(: untag/comp (Tag -> Prim-Computation))
(define ((untag/comp tag) Γ)
  (define K (set-untag Γ tag))
  (prim-preimage Γ K (untag/pre tag)))

(: untag/arr (Tag -> prim-expression))
(define (untag/arr tag)
  (prim-expression (λ () (prim-expression-meaning (untag/fwd tag) (untag/comp tag)))))

;; ===================================================================================================
;; Data type predicates

(define real?/arr (predicate/arr flonum? reals not-reals))
(define null?/arr (predicate/arr null? nulls not-nulls))
(define pair?/arr (predicate/arr pair? pairs not-pairs))
(define boolean?/arr (predicate/arr boolean? bools not-bools))

;; ===================================================================================================
;; Monotone R -> R functions

(: scale/arr (Flonum -> prim-expression))
(define (scale/arr y)
  (cond [(fl= y 0.0)  (c/arr 0.0)]
        [else  (monotone/arr 'scale/arr
                             reals reals (y . fl> . 0.0)
                             (λ: ([x : Flonum]) (fl* x y))
                             (λ: ([x : Flonum]) (fl*/rndd x y))
                             (λ: ([x : Flonum]) (fl*/rndu x y))
                             (λ: ([z : Flonum]) (fl//rndd z y))
                             (λ: ([z : Flonum]) (fl//rndu z y)))]))

(: translate/arr (Flonum -> prim-expression))
(define (translate/arr y)
  (monotone/arr 'translate/arr
                reals reals #t
                (λ: ([x : Flonum]) (fl+ x y))
                (λ: ([x : Flonum]) (fl+/rndd x y))
                (λ: ([x : Flonum]) (fl+/rndu x y))
                (λ: ([z : Flonum]) (fl-/rndd z y))
                (λ: ([z : Flonum]) (fl-/rndu z y))))

(: flneg (Flonum -> Flonum))
(define (flneg x) (fl* -1.0 x))

(: flsqr (Flonum -> Flonum))
(define (flsqr x) (fl* x x))

(: flrecip (Flonum -> Flonum))
(define (flrecip x) (fl/ 1.0 x))

(: flneg-sqrt/rndd (Flonum -> Flonum))
(define (flneg-sqrt/rndd x) (flneg (flsqrt/rndu x)))

(: flneg-sqrt/rndu (Flonum -> Flonum))
(define (flneg-sqrt/rndu x) (flneg (flsqrt/rndd x)))

(define neg/arr (monotone/arr 'neg/arr
                              reals reals #f
                              flneg flneg flneg flneg flneg))

(define exp/arr (monotone/arr 'exp/arr
                              reals nonnegative-interval #t
                              flexp
                              flexp/rndd
                              flexp/rndu
                              fllog/rndd
                              fllog/rndu))

(define log/arr (monotone/arr 'log/arr
                              nonnegative-interval reals #t
                              fllog
                              fllog/rndd
                              fllog/rndu
                              flexp/rndd
                              flexp/rndu))

(define sqrt/arr (monotone/arr 'sqrt/arr
                               nonnegative-interval nonnegative-interval #t
                               flsqrt
                               flsqrt/rndd
                               flsqrt/rndu
                               flsqr/rndd
                               flsqr/rndu))

(define asin/arr
  (monotone/arr 'asin/arr
                (Nonextremal-Interval -1.0 1.0 #t #t)
                (Nonextremal-Interval -pi/2/rndd pi/2/rndu #t #t)
                #t
                flasin
                flasin/rndd
                flasin/rndu
                flsin/rndd
                flsin/rndu))

(define acos/arr
  (monotone/arr 'acos/arr
                (Nonextremal-Interval -1.0 1.0 #t #t)
                (Nonextremal-Interval 0.0 pi/rndu #t #t)
                #f
                flacos
                flacos/rndd
                flacos/rndu
                flcos/rndd
                flcos/rndu))

(define mono-sin/arr
  (monotone/arr 'mono-sin/arr
                (Nonextremal-Interval -pi/2/rndd pi/2/rndu #t #t)
                (Nonextremal-Interval -1.0 1.0 #t #t)
                #t
                flsin
                flsin/rndd
                flsin/rndu
                flasin/rndd
                flasin/rndu))

(define mono-cos/arr
  (monotone/arr 'mono-cos/arr
                (Nonextremal-Interval 0.0 pi/rndu #t #t)
                (Nonextremal-Interval -1.0 1.0 #t #t)
                #f
                flcos
                flcos/rndd
                flcos/rndu
                flacos/rndd
                flacos/rndu))

(define pos-recip/arr
  (monotone/arr 'recip/arr positive-interval positive-interval #f
                flrecip
                flrecip/rndd
                flrecip/rndu
                flrecip/rndd
                flrecip/rndu))

(define neg-recip/arr
  (monotone/arr 'recip/arr negative-interval negative-interval #f
                flrecip
                flrecip/rndd
                flrecip/rndu
                flrecip/rndd
                flrecip/rndu))

(define pos-sqr/arr
  (monotone/arr 'sqr/arr nonnegative-interval nonnegative-interval #t
                flsqr
                flsqr/rndd
                flsqr/rndu
                flsqrt/rndd
                flsqrt/rndu))

(define neg-sqr/arr
  (monotone/arr 'sqr/arr negative-interval positive-interval #f
                flsqr
                flsqr/rndd
                flsqr/rndu
                flneg-sqrt/rndd
                flneg-sqrt/rndu))

(: inverse-cdf/arr (Symbol Nonempty-Interval (Flonum -> Flonum) Index (Flonum -> Flonum) Index
                           -> prim-expression))
(define (inverse-cdf/arr name range inv-cdf inv-ulp-error cdf ulp-error)
  (define-values (a b _a? _b?) (interval-fields range))
  (define-values (inv-cdf/rndd inv-cdf/rndu)
    (make-unary-flops/fake-rnd inv-cdf inv-ulp-error inv-ulp-error a b))
  (define-values (cdf/rndd cdf/rndu)
    (make-unary-flops/fake-rnd cdf ulp-error ulp-error 0.0 1.0))
  (monotone/arr name unit-interval range #t inv-cdf inv-cdf/rndd inv-cdf/rndu cdf/rndd cdf/rndu))


(: cauchy-inv-cdf (Flonum -> Flonum))
(define (cauchy-inv-cdf p)
  (flcauchy-inv-cdf 0.0 1.0 p #f #f))

(: cauchy-cdf (Flonum -> Flonum))
(define (cauchy-cdf x)
  (flcauchy-cdf 0.0 1.0 x #f #f))

(define cauchy/arr (inverse-cdf/arr 'cauchy/arr reals cauchy-inv-cdf 2 cauchy-cdf 2))


(: normal-inv-cdf (Flonum -> Flonum))
(define (normal-inv-cdf p)
  (flnormal-inv-cdf 0.0 1.0 p #f #f))

(: normal-cdf (Flonum -> Flonum))
(define (normal-cdf x)
  (flnormal-cdf 0.0 1.0 x #f #f))

(define normal/arr (inverse-cdf/arr 'normal/arr reals normal-inv-cdf 4 normal-cdf 4))

;; ===================================================================================================
;; Monotone R x R -> R functions

(define +/arr
  (monotone2d/arr '+/arr
                  reals reals reals
                  #t #t fl+ fl+/rndd fl+/rndu
                  #t #f fl-/rndd fl-/rndu
                  #t #f fl-/rndd fl-/rndu))

(: neg-fl-/rndd (Flonum Flonum -> Flonum))
(define (neg-fl-/rndd z x) (fl-/rndd x z))

(: neg-fl-/rndu (Flonum Flonum -> Flonum))
(define (neg-fl-/rndu z x) (fl-/rndu x z))

(define -/arr
  (monotone2d/arr '-/arr
                  reals reals reals
                  #t #f fl- fl-/rndd fl-/rndu
                  #t #t fl+/rndd fl+/rndu
                  #f #t neg-fl-/rndd neg-fl-/rndu))

(define pos-pos-mul/arr
  (monotone2d/arr '*/arr
                  nonnegative-interval nonnegative-interval nonnegative-interval
                  #t #t fl* fl*/rndd fl*/rndu
                  #t #f fl//rndd fl//rndu
                  #t #f fl//rndd fl//rndu))

(define pos-neg-mul/arr
  (monotone2d/arr '*/arr
                  nonnegative-interval negative-interval nonpositive-interval
                  #f #t fl* fl*/rndd fl*/rndu
                  #f #t fl//rndd fl//rndu
                  #t #t fl//rndd fl//rndu))

(define neg-pos-mul/arr
  (monotone2d/arr '*/arr
                  negative-interval nonnegative-interval nonpositive-interval
                  #t #f fl* fl*/rndd fl*/rndu
                  #t #t fl//rndd fl//rndu
                  #f #t fl//rndd fl//rndu))

(define neg-neg-mul/arr
  (monotone2d/arr '*/arr
                  negative-interval negative-interval positive-interval
                  #f #f fl* fl*/rndd fl*/rndu
                  #f #f fl//rndd fl//rndu
                  #f #f fl//rndd fl//rndu))

(: recip-fl//rndd (Flonum Flonum -> Flonum))
(define (recip-fl//rndd z x) (fl//rndd x z))

(: recip-fl//rndu (Flonum Flonum -> Flonum))
(define (recip-fl//rndu z x) (fl//rndu x z))

(define pos-pos-div/arr
  (monotone2d/arr '//arr
                  positive-interval positive-interval positive-interval
                  #t #f fl/ fl//rndd fl//rndu
                  #t #t fl*/rndd fl*/rndu
                  #f #t recip-fl//rndd recip-fl//rndu))

(define pos-neg-div/arr
  (monotone2d/arr '//arr
                  positive-interval negative-interval negative-interval
                  #f #f fl/ fl//rndd fl//rndu
                  #f #f fl*/rndd fl*/rndu
                  #f #f recip-fl//rndd recip-fl//rndu))

(define neg-pos-div/arr
  (monotone2d/arr '//arr
                  negative-interval positive-interval negative-interval
                  #t #t fl/ fl//rndd fl//rndu
                  #t #f fl*/rndd fl*/rndu
                  #t #f recip-fl//rndd recip-fl//rndu))

(define neg-neg-div/arr
  (monotone2d/arr '//arr
                  negative-interval negative-interval positive-interval
                  #f #t fl/ fl//rndd fl//rndu
                  #f #t fl*/rndd fl*/rndu
                  #t #t recip-fl//rndd recip-fl//rndu))

;; ===================================================================================================
;; Real predicates

(: real-predicate/arr (Symbol (Flonum -> Boolean) Nonempty-Real-Set Nonempty-Real-Set
                              -> prim-expression))
(define (real-predicate/arr name p? Γt Γf)
  (predicate/arr (λ: ([γ : Value])
                   (if (flonum? γ) (p? γ) (raise-argument-error name "Flonum" γ)))
                 (bot-basic Γt)
                 (bot-basic Γf)))

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

;; Absolute value
(define abs/arr
  (prim-if/arr negative?/arr neg/arr id/arr))

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
Sine and cosine arrows are direct translations of the following Racket functions:

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

(define real-pair (set-pair reals reals))

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
