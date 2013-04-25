#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         (only-in racket/math pi)
         math/flonum
         math/distributions
         "../set.rkt"
         "arrow-common.rkt"
         "directed-rounding-flops.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Primitive expressions

(define-type Prim-Forward-Fun (Value -> Maybe-Value))
(define-type Prim-Preimage-Fun (Nonempty-Set -> Set))
(define-type Prim-Image-Fun (Nonempty-Set -> Set))

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

(: run-prim-forward (Prim-Forward-Fun Maybe-Value -> Maybe-Value))
(define (run-prim-forward f γ)
  (cond [(bottom? γ)  γ]
        [else  (f γ)]))

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
(define (bottom/fwd γ) (bottom (delay "fail")))

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
(define ((c/pre Γ) K) Γ)

(: c/comp (Nonempty-Set Nonempty-Set -> Prim-Computation))
(define (c/comp domain X)
  (cached-prim-computation
   domain
   (λ (Γ) (prim-computation-meaning X (prim-preimage Γ X (c/pre Γ))))))

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

(: late-boolean/pre (Nonempty-Set -> Prim-Preimage-Fun))
(define ((late-boolean/pre Γ) K) Γ)

(: late-boolean/comp Prim-Computation)
(define late-boolean/comp
  (cached-prim-computation
   universe
   (λ (Γ) (prim-computation-meaning booleans (prim-preimage Γ booleans (late-boolean/pre Γ))))))

(: late-boolean/arr (Flonum -> prim-expression))
(define (late-boolean/arr p)
  (prim-expression (λ () (prim-expression-meaning (late-boolean/fwd p) late-boolean/comp))))

;; ===================================================================================================
;; Arrow composition (reverse composition)

(: prim-rcompose/fwd (Prim-Forward-Fun Prim-Forward-Fun -> Prim-Forward-Fun))
(define ((prim-rcompose/fwd f-fwd g-fwd) γ)
  (let* ([kf  (f-fwd γ)]
         [kg  (run-prim-forward g-fwd kf)])
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
  (define k1 (fst-fwd γ))
  (cond [(bottom? k1)  k1]
        [else  (define k2 (snd-fwd γ))
               (cond [(bottom? k2)  k2]
                     [else  (cons k1 k2)])]))

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
  (cond [(bottom? kc)  kc]
        [(eq? kc #t)  (t-fwd γ)]
        [(eq? kc #f)  (f-fwd γ)]
        [else  (bottom (delay (format "prim-if: expected Boolean condition; given ~e" kc)))]))

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

(: monotone/fwd (Symbol Interval* Interval* (Flonum -> Flonum) -> Prim-Forward-Fun))
(define ((monotone/fwd name f-domain f-range f) γ)
  (cond [(or (not (flonum? γ)) (not (interval*-member? f-domain γ)))
         (bottom (delay (format "~a: expected argument in ~a; given ~e" name f-domain γ)))]
        [else
         (define k (f γ))
         (cond [(not (interval*-member? f-range k))
                (bottom (delay (format "~a: expected result in ~a; produced ~e" name f-range k)))]
               [else  k])]))

(: monotone-apply (Boolean (Flonum -> Flonum) (Flonum -> Flonum) Interval -> Maybe-Interval))
(define (monotone-apply inc? f/rndd f/rndu X)
  (match-define (interval a b a? b?) X)
  (cond [inc?  (interval (f/rndd a) (f/rndu b) a? b?)]
        [else  (interval (f/rndd b) (f/rndu a) b? a?)]))

(: monotone/img (Nonempty-Set Boolean (Flonum -> Flonum) (Flonum -> Flonum) -> Prim-Image-Fun))
(define ((monotone/img K inc? f/rndd f/rndu) Γ)
  (set-intersect
    K (interval*-map (λ (Γ) (monotone-apply inc? f/rndd f/rndu Γ))
                     (assert Γ interval*?))))

(: monotone/pre (Nonempty-Set Boolean (Flonum -> Flonum) (Flonum -> Flonum) -> Prim-Preimage-Fun))
(define ((monotone/pre Γ inc? g/rndd g/rndu) K)
  (set-intersect
   Γ (interval*-map (λ (K) (monotone-apply inc? g/rndd g/rndu K))
                    (assert K interval*?))))

(: monotone/comp (Interval* Interval* Boolean
                            (Flonum -> Flonum)
                            (Flonum -> Flonum) (Flonum -> Flonum)
                            (Flonum -> Flonum) (Flonum -> Flonum)
                            -> Prim-Computation))
(define (monotone/comp f-domain f-range inc? f f/rndd f/rndu g/rndd g/rndu)
  (define img (monotone/img f-range inc? f/rndd f/rndu))
  (cached-prim-computation
   f-domain
   (λ (Γ)
     (define K (img Γ))
     (cond [(empty-set? K)  empty-meaning]
           [else  (define pre (prim-preimage Γ K (monotone/pre Γ inc? g/rndd g/rndu)))
                  (prim-computation-meaning K pre)]))))

(: monotone/arr (Symbol Interval* Interval* Boolean
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

(: monotone2d/fwd (Symbol Pair-Rect Interval* (Flonum Flonum -> Flonum) -> Prim-Forward-Fun))
(define ((monotone2d/fwd name f-domain f-range f) γ)
  (cond [(not (set-member? f-domain γ))
         (bottom (delay (format "~a: expected argument in ~a; given ~e" name f-domain γ)))]
        [else
         (match-define (cons (? flonum? x) (? flonum? y)) γ)
         (define k (f x y))
         (cond [(not (interval*-member? f-range k))
                (bottom (delay (format "~a: expected result in ~a; produced ~e" name f-range k)))]
               [else  k])]))

(: monotone2d-apply (Boolean Boolean
                             (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                             Interval Interval -> Maybe-Interval))
(define (monotone2d-apply xinc? yinc? f/rndd f/rndu X Y)
  (let-values ([(xa xb xa? xb?)  (match-let ([(interval xa xb xa? xb?)  X])
                                   (cond [xinc?  (values xa xb xa? xb?)]
                                         [else   (values xb xa xb? xa?)]))]
               [(ya yb ya? yb?)  (match-let ([(interval ya yb ya? yb?)  Y])
                                   (cond [yinc?  (values ya yb ya? yb?)]
                                         [else   (values yb ya yb? ya?)]))])
    (interval (f/rndd xa ya) (f/rndu xb yb) (and xa? ya?) (and xb? yb?))))

(: monotone2d/img (Nonempty-Set Boolean Boolean
                                (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                                -> Prim-Image-Fun))
(define ((monotone2d/img K xinc? yinc? f/rndd f/rndu) Γ)
  (match-define (pair-rect Γx Γy) Γ)
  (let ([Γx  (assert Γx interval*?)]
        [Γy  (assert Γy interval*?)])
    (set-intersect
     K (interval*-map
        (λ (Γx) (interval*-map
                 (λ (Γy) (monotone2d-apply xinc? yinc? f/rndd f/rndu Γx Γy))
                 Γy))
        Γx))))

(: monotone2d/pre (Nonempty-Set
                   Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                   Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                   -> Prim-Preimage-Fun))
(define (monotone2d/pre Γ gz? gy? g/rndd g/rndu hz? hx? h/rndd h/rndu)
  (match-define (pair-rect orig-Γx orig-Γy) Γ)
  (define Γx (assert orig-Γx interval*?))
  (define Γy (assert orig-Γy interval*?))
  (λ (orig-K)
    (define K (assert orig-K interval*?))
    (define X
      (interval*-map
       (λ (K) (interval*-map
               (λ (Γy) (monotone2d-apply gz? gy? g/rndd g/rndu K Γy))
               Γy))
       K))
    (define Y
      (interval*-map
       (λ (K) (interval*-map
               (λ (Γx) (monotone2d-apply hz? hx? h/rndd h/rndu K Γx))
               Γx))
       K))
    (set-intersect Γ (set-pair X Y))))

(: monotone2d/comp (Pair-Rect Interval*
                              Boolean Boolean (Flonum Flonum -> Flonum)
                              (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                              Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                              Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                              -> Prim-Computation))
(define (monotone2d/comp f-domain f-range
                         fx? fy? f f/rndd f/rndu
                         gz? gy? g/rndd g/rndu
                         hz? hx? h/rndd h/rndu)
  (define img (monotone2d/img f-range fx? fy? f/rndd f/rndu))
  (cached-prim-computation
   f-domain
   (λ (Γ)
     (define K (img Γ))
     (cond [(empty-set? K)  empty-meaning]
           [else  (define pre (prim-preimage Γ K (monotone2d/pre Γ
                                                                 gz? gy? g/rndd g/rndu
                                                                 hz? hx? h/rndd h/rndu)))
                  (prim-computation-meaning K pre)]))))

(: monotone2d/arr (Symbol Pair-Rect Interval*
                          Boolean Boolean (Flonum Flonum -> Flonum)
                          (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          -> prim-expression))
(define (monotone2d/arr name f-domain f-range
                        fx? fy? f f/rndd f/rndu
                        gz? gy? g/rndd g/rndu
                        hz? hx? h/rndd h/rndu)
  (prim-expression
   (λ () (prim-expression-meaning (monotone2d/fwd name f-domain f-range f)
                                  (monotone2d/comp f-domain f-range
                                                   fx? fy? f f/rndd f/rndu
                                                   gz? gy? g/rndd g/rndu
                                                   hz? hx? h/rndd h/rndu)))))

;; ===================================================================================================
;; Predicates

(: predicate/fwd ((Value -> Boolean) -> Prim-Forward-Fun))
(define ((predicate/fwd pred?) γ) (pred? γ))

(: predicate-range (Set Set -> (U Empty-Set Boolean-Rect)))
(define (predicate-range Γt Γf)
  (booleans->boolean-rect (not (empty-set? Γt)) (not (empty-set? Γf))))

(: predicate/pre (Set Set -> Prim-Preimage-Fun))
(define ((predicate/pre Γt Γf) B)
  (cond [(eq? B booleans)  (set-join Γt Γf)]
        [(eq? B trues)     Γt]
        [(eq? B falses)    Γf]
        [else              empty-set]))

(: predicate/comp (Nonempty-Set Nonempty-Set -> Prim-Computation))
(define (predicate/comp Γt Γf)
  (cached-prim-computation
   (set-join Γt Γf)
   (λ (Γ)
     (let ([Γt  (set-intersect Γ Γt)]
           [Γf  (set-intersect Γ Γf)])
       (define K (predicate-range Γt Γf))
       (cond [(empty-set? K)  empty-meaning]
             [else  (define pre (prim-preimage Γ K (predicate/pre Γt Γf)))
                    (prim-computation-meaning K pre)])))))

(: predicate/arr ((Value -> Boolean) Nonempty-Set Nonempty-Set -> prim-expression))
(define (predicate/arr pred? Γt Γf)
  (prim-expression
   (λ () (prim-expression-meaning (predicate/fwd pred?)
                                  (predicate/comp Γt Γf)))))

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
      (bottom (delay (format "expected ~e; given ~e" tag γ)))))

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

(define real?/arr (predicate/arr flonum? real-interval not-reals))
(define null?/arr (predicate/arr null? null-rect not-null-set))
(define pair?/arr (predicate/arr pair? all-pairs not-pairs))
(define boolean?/arr (predicate/arr boolean? booleans not-booleans))

;; ===================================================================================================
;; Monotone R -> R functions

(: scale/arr (Flonum -> prim-expression))
(define (scale/arr y)
  (cond [(fl= y 0.0)  (c/arr 0.0)]
        [else  (monotone/arr 'scale/arr real-interval real-interval (y . fl> . 0.0)
                             (λ: ([x : Flonum]) (fl* x y))
                             (λ: ([x : Flonum]) (fl*/rndd x y))
                             (λ: ([x : Flonum]) (fl*/rndu x y))
                             (λ: ([z : Flonum]) (fl//rndd z y))
                             (λ: ([z : Flonum]) (fl//rndu z y)))]))

(: translate/arr (Flonum -> prim-expression))
(define (translate/arr y)
  (monotone/arr 'translate/arr real-interval real-interval #t
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
                              real-interval real-interval #f
                              flneg flneg flneg flneg flneg))

(define exp/arr (monotone/arr 'exp/arr
                              real-interval nonnegative-interval #t
                              flexp
                              flexp/rndd
                              flexp/rndu
                              fllog/rndd
                              fllog/rndu))

(define log/arr (monotone/arr 'log/arr
                              nonnegative-interval real-interval #t
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
                (Interval -1.0 1.0 #t #t) (Interval -pi/2/rndd pi/2/rndu #t #t) #t
                flasin
                flasin/rndd
                flasin/rndu
                flsin/rndd
                flsin/rndu))

(define acos/arr
  (monotone/arr 'acos/arr
                (Interval -1.0 1.0 #t #t) (Interval 0.0 pi/rndu #t #t) #f
                flacos
                flacos/rndd
                flacos/rndu
                flcos/rndd
                flcos/rndu))

(define mono-sin/arr
  (monotone/arr 'mono-sin/arr
                (Interval -pi/2/rndd pi/2/rndu #t #t) (Interval -1.0 1.0 #t #t) #t
                flsin
                flsin/rndd
                flsin/rndu
                flasin/rndd
                flasin/rndu))

(define mono-cos/arr
  (monotone/arr 'mono-cos/arr
                (Interval 0.0 pi/rndu #t #t) (Interval -1.0 1.0 #t #t) #f
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

(: inverse-cdf/arr (Symbol Interval (Flonum -> Flonum) Index (Flonum -> Flonum) Index
                           -> prim-expression))
(define (inverse-cdf/arr name range inv-cdf inv-ulp-error cdf ulp-error)
  (match-define (interval a b _a? _b?) range)
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

(define cauchy/arr (inverse-cdf/arr 'cauchy/arr real-interval cauchy-inv-cdf 2 cauchy-cdf 2))


(: normal-inv-cdf (Flonum -> Flonum))
(define (normal-inv-cdf p)
  (flnormal-inv-cdf 0.0 1.0 p #f #f))

(: normal-cdf (Flonum -> Flonum))
(define (normal-cdf x)
  (flnormal-cdf 0.0 1.0 x #f #f))

(define normal/arr (inverse-cdf/arr 'normal/arr real-interval normal-inv-cdf 4 normal-cdf 4))

;; ===================================================================================================
;; Monotone R x R -> R functions

(define real-pair (pair-rect real-interval real-interval))

(define +/arr
  (monotone2d/arr '+/arr
                  real-pair real-interval
                  #t #t fl+ fl+/rndd fl+/rndu
                  #t #f fl-/rndd fl-/rndu
                  #t #f fl-/rndd fl-/rndu))

(: neg-fl-/rndd (Flonum Flonum -> Flonum))
(define (neg-fl-/rndd z x) (fl-/rndd x z))

(: neg-fl-/rndu (Flonum Flonum -> Flonum))
(define (neg-fl-/rndu z x) (fl-/rndu x z))

(define -/arr
  (monotone2d/arr '-/arr
                  real-pair real-interval
                  #t #f fl- fl-/rndd fl-/rndu
                  #t #t fl+/rndd fl+/rndu
                  #f #t neg-fl-/rndd neg-fl-/rndu))

(define pos-pos-mul/arr
  (monotone2d/arr '*/arr
                  (pair-rect nonnegative-interval nonnegative-interval) nonnegative-interval
                  #t #t fl* fl*/rndd fl*/rndu
                  #t #f fl//rndd fl//rndu
                  #t #f fl//rndd fl//rndu))

(define pos-neg-mul/arr
  (monotone2d/arr '*/arr
                  (pair-rect nonnegative-interval negative-interval) nonpositive-interval
                  #f #t fl* fl*/rndd fl*/rndu
                  #f #t fl//rndd fl//rndu
                  #t #t fl//rndd fl//rndu))

(define neg-pos-mul/arr
  (monotone2d/arr '*/arr
                  (pair-rect negative-interval nonnegative-interval) nonpositive-interval
                  #t #f fl* fl*/rndd fl*/rndu
                  #t #t fl//rndd fl//rndu
                  #f #t fl//rndd fl//rndu))

(define neg-neg-mul/arr
  (monotone2d/arr '*/arr
                  (pair-rect negative-interval negative-interval) positive-interval
                  #f #f fl* fl*/rndd fl*/rndu
                  #f #f fl//rndd fl//rndu
                  #f #f fl//rndd fl//rndu))

(: recip-fl//rndd (Flonum Flonum -> Flonum))
(define (recip-fl//rndd z x) (fl//rndd x z))

(: recip-fl//rndu (Flonum Flonum -> Flonum))
(define (recip-fl//rndu z x) (fl//rndu x z))

(define pos-pos-div/arr
  (monotone2d/arr '//arr
                  (pair-rect positive-interval positive-interval) positive-interval
                  #t #f fl/ fl//rndd fl//rndu
                  #t #t fl*/rndd fl*/rndu
                  #f #t recip-fl//rndd recip-fl//rndu))

(define pos-neg-div/arr
  (monotone2d/arr '//arr
                  (pair-rect positive-interval negative-interval) negative-interval
                  #f #f fl/ fl//rndd fl//rndu
                  #f #f fl*/rndd fl*/rndu
                  #f #f recip-fl//rndd recip-fl//rndu))

(define neg-pos-div/arr
  (monotone2d/arr '//arr
                  (pair-rect negative-interval positive-interval) negative-interval
                  #t #t fl/ fl//rndd fl//rndu
                  #t #f fl*/rndd fl*/rndu
                  #t #f recip-fl//rndd recip-fl//rndu))

(define neg-neg-div/arr
  (monotone2d/arr '//arr
                  (pair-rect negative-interval negative-interval) positive-interval
                  #f #t fl/ fl//rndd fl//rndu
                  #f #t fl*/rndd fl*/rndu
                  #t #t recip-fl//rndd recip-fl//rndu))

;; ===================================================================================================
;; Real predicates

(: real-predicate/arr (Symbol (Flonum -> Boolean) Interval* Interval* -> prim-expression))
(define (real-predicate/arr name p? Γt Γf)
  (predicate/arr (λ: ([γ : Value])
                   (if (flonum? γ) (p? γ) (raise-argument-error name "Flonum" γ)))
                 Γt Γf))

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
