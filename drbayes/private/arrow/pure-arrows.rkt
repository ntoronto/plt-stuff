#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         (only-in racket/math pi)
         math/flonum
         math/distributions
         "../set.rkt"
         "../untyped-utils.rkt"
         "directed-rounding-flops.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage mappings

(define-singleton-type Empty-Pre-Mapping empty-pre-mapping)

(struct: nonempty-pre-mapping ([range : Nonempty-Set]
                               [fun : (Nonempty-Set -> Set)])
  #:transparent)

(define-type Pre-Mapping (U Empty-Pre-Mapping nonempty-pre-mapping))

(: pre-mapping (Set (Nonempty-Set -> Set) -> Pre-Mapping))
(define (pre-mapping Y p)
  (cond [(empty-set? Y)  empty-pre-mapping]
        [else  (nonempty-pre-mapping Y p)]))

(: ap/pre (Pre-Mapping Set -> Set))
(define (ap/pre h B)
  (match h
    [(? empty-pre-mapping?)      empty-set]
    [(nonempty-pre-mapping Y p)  (let ([B  (set-intersect B Y)])
                                   (if (empty-set? B) empty-set (p B)))]))

(: range/pre (Pre-Mapping -> Set))
(define (range/pre h)
  (cond [(empty-pre-mapping? h)  empty-set]
        [else  (nonempty-pre-mapping-range h)]))

(: compose/pre (Pre-Mapping Pre-Mapping -> Pre-Mapping))
(define (compose/pre h2 h1)
  (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? h2))  empty-pre-mapping]
        [else  (match-define (nonempty-pre-mapping Z p2) h2)
               (nonempty-pre-mapping Z (λ (C) (ap/pre h1 (p2 C))))]))

(: pair/pre (Pre-Mapping Pre-Mapping -> Pre-Mapping))
(define (pair/pre h1 h2)
  (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? h2))  empty-pre-mapping]
        [else  (match-define (nonempty-pre-mapping Y1 p1) h1)
               (match-define (nonempty-pre-mapping Y2 p2) h2)
               (nonempty-pre-mapping
                (set-pair Y1 Y2)
                (λ (B)
                  (define-values (B1 B2) (set-projs B))
                  (cond [(or (empty-set? B1) (empty-set? B2))  empty-set]
                        [else  (define A1 (p1 B1))
                               (cond [(empty-set? A1)  empty-set]
                                     [else  (set-intersect A1 (p2 B2))])])))]))

(: uplus/pre (Pre-Mapping Pre-Mapping -> Pre-Mapping))
(define (uplus/pre h1 h2)
  (cond [(empty-pre-mapping? h1)  h2]
        [(empty-pre-mapping? h2)  h1]
        [else  (nonempty-pre-mapping (set-join (nonempty-pre-mapping-range h1)
                                               (nonempty-pre-mapping-range h2))
                                     (λ (B) (set-join (ap/pre h1 B)
                                                      (ap/pre h2 B))))]))

;; ===================================================================================================
;; Arrows

(define-type Bot-Arrow (Value -> Maybe-Value))
(define-type Pre-Arrow (Nonempty-Set -> Pre-Mapping))

(struct: interps ([forward : Bot-Arrow]
                  [backward : Pre-Arrow])
  #:transparent)

(struct: prim-arrow ([interps : (-> interps)])
  #:transparent)

(: run/pre (case-> (Pre-Arrow Empty-Set -> Empty-Pre-Mapping)
                   (Pre-Arrow Set -> Pre-Mapping)))
(define (run/pre h A)
  (cond [(empty-set? A)  empty-pre-mapping]
        [else  (h A)]))

;; ===================================================================================================
;; Basic computable lifts

;; ---------------------------------------------------------------------------------------------------
;; Bottom

(: bottom/bot (Value -> Bottom))
(define (bottom/bot a) (bottom (delay "⊥")))

(: bottom/pre Pre-Arrow)
(define (bottom/pre A) empty-pre-mapping)

(define bottom/prim
  (prim-arrow (λ () (interps bottom/bot bottom/pre))))

;; ---------------------------------------------------------------------------------------------------
;; Identity function

(: id/bot (Value -> Value))
(define (id/bot a) a)

(: id/pre Pre-Arrow)
(define (id/pre A)
  (nonempty-pre-mapping A (λ (B) B)))

(define id/prim
  (prim-arrow (λ () (interps id/bot id/pre))))

;; ---------------------------------------------------------------------------------------------------
;; Constant functions

(: const/bot (Value -> (Value -> Value)))
(define ((const/bot b) a) b)

(: const/pre (Nonempty-Set Nonempty-Set -> Pre-Arrow))
(define ((const/pre Y X) A)
  (let ([A  (set-intersect A X)])
    (cond [(empty-set? A)  empty-pre-mapping]
          [else  (nonempty-pre-mapping Y (λ (B) A))])))

(: const/prim (case-> (Value -> prim-arrow)
                      (Value Nonempty-Set -> prim-arrow)))
(define (const/prim b [X universe])
  (prim-arrow (λ () (interps (const/bot b) (const/pre (value->singleton b) X)))))

;; ---------------------------------------------------------------------------------------------------
;; Pair and list projections

(: ref/bot (Pair-Index -> Bot-Arrow))
(define ((ref/bot j) a)
  (value-pair-ref a j))

(: ref/pre (Pair-Index -> Pre-Arrow))
(define ((ref/pre j) A)
  (pre-mapping (set-proj A j) (λ (B) (set-unproj A j B))))

(: ref/prim (Pair-Index -> prim-arrow))
(define (ref/prim j)
  (prim-arrow (λ () (interps (ref/bot j) (ref/pre j)))))

;; ===================================================================================================
;; Arrow combinators (except the uncomputable `arr')

;; ---------------------------------------------------------------------------------------------------
;; Composition

(: >>>/bot (Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((>>>/bot f1 f2) a)
  (define b (f1 a))
  (if (bottom? b) b (f2 b)))

(: >>>/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((>>>/pre h1 h2) A)
  (let* ([h1  (h1 A)]
         [h2  (run/pre h2 (range/pre h1))])
    (compose/pre h2 h1)))

(: >>>/prim (prim-arrow prim-arrow -> prim-arrow))
(define (>>>/prim e1 e2)
  (let ([e1  (prim-arrow-interps e1)]
        [e2  (prim-arrow-interps e2)])
    (prim-arrow (λ ()
                  (match-define (interps f1 h1) (e1))
                  (match-define (interps f2 h2) (e2))
                  (interps (>>>/bot f1 f2) (>>>/pre h1 h2))))))

;; ---------------------------------------------------------------------------------------------------
;; Pairing

(: &&&/bot (Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((&&&/bot f1 f2) a)
  (define b1 (f1 a))
  (cond [(bottom? b1)  b1]
        [else  (define b2 (f2 a))
               (cond [(bottom? b2)  b2]
                     [else  (cons b1 b2)])]))

(: &&&/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((&&&/pre h1 h2) A)
  (let ([h1  (h1 A)]
        [h2  (h2 A)])
    (pair/pre h1 h2)))

(: &&&/prim (prim-arrow prim-arrow -> prim-arrow))
(define (&&&/prim e1 e2)
  (let ([e1  (prim-arrow-interps e1)]
        [e2  (prim-arrow-interps e2)])
    (prim-arrow (λ ()
                  (match-define (interps f1 h1) (e1))
                  (match-define (interps f2 h2) (e2))
                  (interps (&&&/bot f1 f2) (&&&/pre h1 h2))))))

;; ---------------------------------------------------------------------------------------------------
;; Partial if-then-else

(: ifte/bot (Bot-Arrow Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((ifte/bot f1 f2 f3) a)
  (define b (f1 a))
  (cond [(bottom? b)  b]
        [(eq? b #t)  (f2 a)]
        [(eq? b #f)  (f3 a)]
        [else  (bottom (delay (format "ifte/bot: expected Boolean condition; given ~e" b)))]))

(: ifte/pre (Pre-Arrow Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((ifte/pre h1 h2 h3) A)
  (let* ([h1  (h1 A)]
         [h2  (run/pre h2 (ap/pre h1 trues))]
         [h3  (run/pre h3 (ap/pre h1 falses))])
    (uplus/pre h2 h3)))

(: ifte/prim (prim-arrow prim-arrow prim-arrow -> prim-arrow))
(define (ifte/prim e1 e2 e3)
  (let ([e1  (prim-arrow-interps e1)]
        [e2  (prim-arrow-interps e2)]
        [e3  (prim-arrow-interps e3)])
    (prim-arrow (λ ()
                  (match-define (interps f1 h1) (e1))
                  (match-define (interps f2 h2) (e2))
                  (match-define (interps f3 h3) (e3))
                  (interps (ifte/bot f1 f2 f3) (ifte/pre h1 h2 h3))))))

;; ---------------------------------------------------------------------------------------------------
;; Laziness

(: lazy/bot ((-> Bot-Arrow) -> Bot-Arrow))
(define ((lazy/bot f) a) ((f) a))

(: lazy/pre ((-> Pre-Arrow) -> Pre-Arrow))
(define ((lazy/pre h) A) ((h) A))

(: lazy/prim ((-> prim-arrow) -> prim-arrow))
(define (lazy/prim e)
  (prim-arrow (λ ()
                (define ints (delay ((prim-arrow-interps (e)))))
                (interps (lazy/bot (λ () (interps-forward (force ints))))
                         (lazy/pre (λ () (interps-backward (force ints))))))))

#|
These expressions should all terminate

(: halt-on-true/bot Bot-Arrow)
(define halt-on-true/bot
  (ifte/bot id/bot id/bot (lazy/bot (λ () halt-on-true/bot))))
(halt-on-true/bot #t)

(: halt-on-true/pre Pre-Arrow)
(define halt-on-true/pre
  (ifte/pre id/pre id/pre (lazy/pre (λ () halt-on-true/pre))))
(halt-on-true/pre trues)
(ap/pre (halt-on-true/pre trues) trues)
(ap/pre (halt-on-true/pre trues) falses)

(: halt-on-true/prim prim-arrow)
(define halt-on-true/prim
  (ifte/prim id/prim id/prim (lazy/prim (λ () halt-on-true/prim))))
((interps-forward ((prim-arrow-interps halt-on-true/prim))) #t)
((interps-backward ((prim-arrow-interps halt-on-true/prim))) trues)
(ap/pre ((interps-backward ((prim-arrow-interps halt-on-true/prim))) trues) trues)
(ap/pre ((interps-backward ((prim-arrow-interps halt-on-true/prim))) trues) falses)
|#

;; ===================================================================================================
;; Monotone R -> R lifts

(: monotone/bot (Symbol Nonempty-Real-Set Nonempty-Real-Set (Flonum -> Flonum) -> Bot-Arrow))
(define ((monotone/bot name X Y f) a)
  (cond [(or (not (flonum? a)) (not (real-set-member? X a)))
         (bottom (delay (format "~a: expected argument in ~e; given ~e" name X a)))]
        [else
         (define b (f a))
         (cond [(not (real-set-member? Y b))
                (bottom (delay (format "~a: expected result in ~e; produced ~e" name Y b)))]
               [else  b])]))

(: monotone-apply (Boolean (Flonum -> Flonum) (Flonum -> Flonum) Set -> Set))
(define (monotone-apply inc? f/rndd f/rndu A)
  (define B
    (real-set-map (λ (A)
                    (define-values (a1 a2 a1? a2?) (interval-fields A))
                    (cond [inc?  (interval (f/rndd a1) (f/rndu a2) a1? a2?)]
                          [else  (interval (f/rndd a2) (f/rndu a1) a2? a1?)]))
                  (set-take-reals A)))
  (if (empty-real-set? B) empty-set B))

(: monotone/pre (Nonempty-Real-Set
                 Nonempty-Real-Set
                 Boolean
                 (Flonum -> Flonum) (Flonum -> Flonum)
                 (Flonum -> Flonum) (Flonum -> Flonum)
                 -> Pre-Arrow))
(define ((monotone/pre X Y inc? f/rndd f/rndu g/rndd g/rndu) A)
  (pre-mapping (set-intersect Y (monotone-apply inc? f/rndd f/rndu A))
               (λ (B) (set-intersect X (monotone-apply inc? g/rndd g/rndu B)))))

(: monotone/prim (Symbol
                  Nonempty-Real-Set
                  Nonempty-Real-Set
                  (Flonum -> Flonum)
                  Boolean
                  (Flonum -> Flonum) (Flonum -> Flonum)
                  (Flonum -> Flonum) (Flonum -> Flonum)
                  -> prim-arrow))
(define (monotone/prim name X Y f inc? f/rndd f/rndu g/rndd g/rndu)
  (prim-arrow (λ () (interps (monotone/bot name X Y f)
                             (monotone/pre X Y inc? f/rndd f/rndu g/rndd g/rndu)))))

;; ===================================================================================================
;; Monotone R x R -> R lifts

(: monotone2d/bot (Symbol
                   Nonempty-Real-Set Nonempty-Real-Set Nonempty-Real-Set (Flonum Flonum -> Flonum)
                   -> Bot-Arrow))
(define ((monotone2d/bot name X1 X2 Y f) a)
  (define (fail)
    (bottom (delay (format "~a: expected argument in ~e; given ~e" name (set-pair X1 X2) a))))
  (match a
    [(cons (? flonum? a1) (? flonum? a2))
     (cond [(and (real-set-member? X1 a1) (real-set-member? X2 a2))  (f a1 a2)]
           [else  (fail)])]
    [_  (fail)]))

(: monotone2d-apply (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                             Nonempty-Interval Nonempty-Interval -> Interval))
(define (monotone2d-apply xinc? yinc? f/rndd f/rndu X Y)
  (let-values ([(x1 x2 x1? x2?)  (let-values ([(x1 x2 x1? x2?)  (interval-fields X)])
                                   (cond [xinc?  (values x1 x2 x1? x2?)]
                                         [else   (values x2 x1 x2? x1?)]))]
               [(y1 y2 y1? y2?)  (let-values ([(y1 y2 y1? y2?)  (interval-fields Y)])
                                   (cond [yinc?  (values y1 y2 y1? y2?)]
                                         [else   (values y2 y1 y2? y1?)]))])
    (interval (f/rndd x1 y1) (f/rndu x2 y2) (and x1? y1?) (and x2? y2?))))

(: monotone2d-image (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                             Nonempty-Set Nonempty-Set -> Set))
(define (monotone2d-image xinc? yinc? f/rndd f/rndu A Y)
  (define-values (A1 A2) (set-projs A))
  (let ([A1  (set-take-reals A1)]
        [A2  (set-take-reals A2)])
    (define B
      (real-set-map (λ (A1) (real-set-map (λ (A2) (monotone2d-apply xinc? yinc? f/rndd f/rndu A1 A2))
                                          A2))
                    A1))
    (cond [(empty-real-set? B)  empty-set]
          [else  (set-intersect Y B)])))

(: monotone2d-preimage (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                                Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                                Nonempty-Set Nonempty-Set -> Set))
(define (monotone2d-preimage gz? gy? g/rndd g/rndu hz? hx? h/rndd h/rndu X B)
  (define-values (X1 X2) (set-projs X))
  (let ([B  (set-take-reals B)]
        [X1  (set-take-reals X1)]
        [X2  (set-take-reals X2)])
    (define A1
      (real-set-map (λ (B) (real-set-map (λ (X2) (monotone2d-apply gz? gy? g/rndd g/rndu B X2))
                                         X2))
                    B))
    (define A2
      (real-set-map (λ (B) (real-set-map (λ (X1) (monotone2d-apply hz? hx? h/rndd h/rndu B X1))
                                         X1))
                    B))
    (set-intersect X (set-pair (bot-basic A1) (bot-basic A2)))))

(: monotone2d/pre (Bot-Basic Nonempty-Real-Set
                             Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                             Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                             Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                             -> Pre-Arrow))
(define ((monotone2d/pre X Y
                         fx? fy? f/rndd f/rndu
                         gz? gy? g/rndd g/rndu
                         hz? hx? h/rndd h/rndu)
         A)
  (pre-mapping (monotone2d-image fx? fy? f/rndd f/rndu A Y)
               (λ (B) (monotone2d-preimage gz? gy? g/rndd g/rndu hz? hx? h/rndd h/rndu X B))))

(: monotone2d/prim (Symbol
                    Nonempty-Real-Set Nonempty-Real-Set Nonempty-Real-Set (Flonum Flonum -> Flonum)
                    Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                    Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                    Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                    -> prim-arrow))
(define (monotone2d/prim name X1 X2 Y f
                         fx? fy? f/rndd f/rndu
                         gz? gy? g/rndd g/rndu
                         hz? hx? h/rndd h/rndu)
  (prim-arrow (λ () (interps (monotone2d/bot name X1 X2 Y f)
                             (monotone2d/pre (set-pair X1 X2) Y
                                             fx? fy? f/rndd f/rndu
                                             gz? gy? g/rndd g/rndu
                                             hz? hx? h/rndd h/rndu)))))

;; ===================================================================================================
;; Predicate lifts

(: predicate/bot (Symbol (Value -> (U Bottom Boolean)) Nonempty-Set Nonempty-Set -> Bot-Arrow))
(define ((predicate/bot name f Xt Xf) a)
  (define b (f a))
  (cond [(bottom? b)  b]
        [(eq? b #t)  (cond [(set-member? Xt a)  #t]
                           [else  (bottom (delay (format "~a: expected argument in ~e; given ~e"
                                                         name Xt a)))])]
        [(eq? b #f)  (cond [(set-member? Xf a)  #f]
                           [else  (bottom (delay (format "~a: expected argument in ~e; given ~e"
                                                         name Xf a)))])]
        [else  (bottom (delay (format "~a: expected Boolean condition; given ~e" name b)))]))

(: predicate/pre (Nonempty-Set Nonempty-Set -> Pre-Arrow))
(define ((predicate/pre Xt Xf) A)
  (define At (set-intersect A Xt))
  (define Af (set-intersect A Xf))
  (cond [(and (empty-set? At) (empty-set? Af))  empty-pre-mapping]
        [(empty-set? Af)  (nonempty-pre-mapping trues  (λ (B) At))]
        [(empty-set? At)  (nonempty-pre-mapping falses (λ (B) Af))]
        [else   (define A (delay (set-join At Af)))
                (nonempty-pre-mapping bools (λ (B) (cond [(trues? B)  At]
                                                         [(falses? B)  Af]
                                                         [else  (force A)])))]))

(: predicate/prim (Symbol (Value -> (U Bottom Boolean)) Nonempty-Set Nonempty-Set -> prim-arrow))
(define (predicate/prim name f Xt Xf)
  (prim-arrow (λ () (interps (predicate/bot name f Xt Xf) (predicate/pre Xt Xf)))))

;; ===================================================================================================
;; Tagged value lifts

;; ---------------------------------------------------------------------------------------------------
;; Tag predicate lifts

(: tag?/bot (Tag -> (Value -> Boolean)))
(define ((tag?/bot tag) a)
  (and (tagged-value? a) (eq? tag (tagged-value-tag a))))

(: tag?/pre (Tag -> Pre-Arrow))
(define (tag?/pre tag)
  (predicate/pre (bot-tagged tag universe) (top-tagged tag empty-set)))

(: tag?/prim (Tag -> prim-arrow))
(define (tag?/prim tag)
  (prim-arrow (λ () (interps (tag?/bot tag) (tag?/pre tag)))))

;; ---------------------------------------------------------------------------------------------------
;; Tagging lifts

(: tag/bot (Tag -> Bot-Arrow))
(define ((tag/bot tag) a) (tagged-value tag a))

(: tag/pre (Tag -> Pre-Arrow))
(define ((tag/pre tag) A)
  (nonempty-pre-mapping (set-tag A tag) (λ (B) (set-untag B tag))))

(: tag/prim (Tag -> prim-arrow))
(define (tag/prim tag)
  (prim-arrow (λ () (interps (tag/bot tag) (tag/pre tag)))))

;; ---------------------------------------------------------------------------------------------------
;; Untagging lifts

(: untag/bot (Tag -> Bot-Arrow))
(define ((untag/bot tag) a)
  (if (and (tagged-value? a) (eq? tag (tagged-value-tag a)))
      (tagged-value-value a)
      (bottom (delay (format "expected ~a; given ~e" tag a)))))

(: untag/pre (Tag -> Pre-Arrow))
(define ((untag/pre tag) A)
  (pre-mapping (set-untag A tag) (λ (B) (set-tag B tag))))

(: untag/prim (Tag -> prim-arrow))
(define (untag/prim tag)
  (prim-arrow (λ () (interps (untag/bot tag) (untag/pre tag)))))

;; ===================================================================================================
;; Computable lifts

;; ---------------------------------------------------------------------------------------------------
;; Data type predicates

(define real?/prim (predicate/prim 'real? flonum? reals not-reals))
(define null?/prim (predicate/prim 'null? null? nulls not-nulls))
(define pair?/prim (predicate/prim 'pair? pair? pairs not-pairs))
(define boolean?/prim (predicate/prim 'boolean? boolean? bools not-bools))

;; ---------------------------------------------------------------------------------------------------
;; Monotone elementary R -> R functions

(: scale/prim (Flonum -> prim-arrow))
(define (scale/prim y)
  (cond [(fl= y 0.0)  (const/prim 0.0)]
        [else  (monotone/prim 'scale reals reals
                              (λ: ([x : Flonum]) (fl* x y))
                              (y . fl> . 0.0)
                              (λ: ([x : Flonum]) (fl*/rndd x y))
                              (λ: ([x : Flonum]) (fl*/rndu x y))
                              (λ: ([z : Flonum]) (fl//rndd z y))
                              (λ: ([z : Flonum]) (fl//rndu z y)))]))

(: translate/prim (Flonum -> prim-arrow))
(define (translate/prim y)
  (monotone/prim 'translate reals reals
                 (λ: ([x : Flonum]) (fl+ x y))
                 #t
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

(define neg/prim (monotone/prim 'neg reals reals flneg #f flneg flneg flneg flneg))

(define exp/prim (monotone/prim 'exp reals nonnegative-interval
                                flexp
                                #t
                                flexp/rndd
                                flexp/rndu
                                fllog/rndd
                                fllog/rndu))

(define log/prim (monotone/prim 'log nonnegative-interval reals
                                fllog
                                #t
                                fllog/rndd
                                fllog/rndu
                                flexp/rndd
                                flexp/rndu))

(define sqrt/prim (monotone/prim 'sqrt nonnegative-interval nonnegative-interval
                                 flsqrt
                                 #t
                                 flsqrt/rndd
                                 flsqrt/rndu
                                 flsqr/rndd
                                 flsqr/rndu))

(define asin/prim
  (monotone/prim 'asin
                 (Nonextremal-Interval -1.0 1.0 #t #t)
                 (Nonextremal-Interval -pi/2/rndd pi/2/rndu #t #t)
                 flasin
                 #t
                 flasin/rndd
                 flasin/rndu
                 flsin/rndd
                 flsin/rndu))

(define acos/prim
  (monotone/prim 'acos
                 (Nonextremal-Interval -1.0 1.0 #t #t)
                 (Nonextremal-Interval 0.0 pi/rndu #t #t)
                 flacos
                 #f
                 flacos/rndd
                 flacos/rndu
                 flcos/rndd
                 flcos/rndu))

(define mono-sin/prim
  (monotone/prim 'mono-sin
                 (Nonextremal-Interval -pi/2/rndd pi/2/rndu #t #t)
                 (Nonextremal-Interval -1.0 1.0 #t #t)
                 flsin
                 #t
                 flsin/rndd
                 flsin/rndu
                 flasin/rndd
                 flasin/rndu))

(define mono-cos/prim
  (monotone/prim 'mono-cos
                 (Nonextremal-Interval 0.0 pi/rndu #t #t)
                 (Nonextremal-Interval -1.0 1.0 #t #t)
                 flcos
                 #f
                 flcos/rndd
                 flcos/rndu
                 flacos/rndd
                 flacos/rndu))

(define pos-recip/prim
  (monotone/prim 'recip positive-interval positive-interval
                 flrecip
                 #f
                 flrecip/rndd
                 flrecip/rndu
                 flrecip/rndd
                 flrecip/rndu))

(define neg-recip/prim
  (monotone/prim 'recip negative-interval negative-interval
                 flrecip
                 #f
                 flrecip/rndd
                 flrecip/rndu
                 flrecip/rndd
                 flrecip/rndu))

(define pos-sqr/prim
  (monotone/prim 'sqr nonnegative-interval nonnegative-interval
                 flsqr
                 #t
                 flsqr/rndd
                 flsqr/rndu
                 flsqrt/rndd
                 flsqrt/rndu))

(define neg-sqr/prim
  (monotone/prim 'sqr negative-interval positive-interval
                 flsqr
                 #f
                 flsqr/rndd
                 flsqr/rndu
                 flneg-sqrt/rndd
                 flneg-sqrt/rndu))

;; ---------------------------------------------------------------------------------------------------
;; Inverse CDFs

(: inverse-cdf/prim (Symbol Nonempty-Interval (Flonum -> Flonum) Index (Flonum -> Flonum) Index
                            -> prim-arrow))
(define (inverse-cdf/prim name Y inv-cdf inv-ulp-error cdf ulp-error)
  (define-values (a1 a2 _a1? _a2?) (interval-fields Y))
  (define-values (inv-cdf/rndd inv-cdf/rndu)
    (make-unary-flops/fake-rnd inv-cdf inv-ulp-error inv-ulp-error a1 a2))
  (define-values (cdf/rndd cdf/rndu)
    (make-unary-flops/fake-rnd cdf ulp-error ulp-error 0.0 1.0))
  (monotone/prim name unit-interval Y inv-cdf #t inv-cdf/rndd inv-cdf/rndu cdf/rndd cdf/rndu))


(: cauchy-inv-cdf (Flonum -> Flonum))
(define (cauchy-inv-cdf p)
  (flcauchy-inv-cdf 0.0 1.0 p #f #f))

(: cauchy-cdf (Flonum -> Flonum))
(define (cauchy-cdf x)
  (flcauchy-cdf 0.0 1.0 x #f #f))

(define cauchy/prim (inverse-cdf/prim 'cauchy reals cauchy-inv-cdf 2 cauchy-cdf 2))


(: normal-inv-cdf (Flonum -> Flonum))
(define (normal-inv-cdf p)
  (flnormal-inv-cdf 0.0 1.0 p #f #f))

(: normal-cdf (Flonum -> Flonum))
(define (normal-cdf x)
  (flnormal-cdf 0.0 1.0 x #f #f))

(define normal/prim (inverse-cdf/prim 'normal reals normal-inv-cdf 4 normal-cdf 4))

;; ---------------------------------------------------------------------------------------------------
;; Monotone arithmetic R x R -> R functions

(define +/prim
  (monotone2d/prim '+ reals reals reals
                   fl+
                   #t #t fl+/rndd fl+/rndu
                   #t #f fl-/rndd fl-/rndu
                   #t #f fl-/rndd fl-/rndu))

(: neg-fl-/rndd (Flonum Flonum -> Flonum))
(define (neg-fl-/rndd z x) (fl-/rndd x z))

(: neg-fl-/rndu (Flonum Flonum -> Flonum))
(define (neg-fl-/rndu z x) (fl-/rndu x z))

(define -/prim
  (monotone2d/prim '- reals reals reals
                   fl-
                   #t #f fl-/rndd fl-/rndu
                   #t #t fl+/rndd fl+/rndu
                   #f #t neg-fl-/rndd neg-fl-/rndu))

(define pos-pos-mul/prim
  (monotone2d/prim '* nonnegative-interval nonnegative-interval nonnegative-interval
                   fl*
                   #t #t fl*/rndd fl*/rndu
                   #t #f fl//rndd fl//rndu
                   #t #f fl//rndd fl//rndu))

(define pos-neg-mul/prim
  (monotone2d/prim '* nonnegative-interval negative-interval nonpositive-interval
                   fl*
                   #f #t fl*/rndd fl*/rndu
                   #f #t fl//rndd fl//rndu
                   #t #t fl//rndd fl//rndu))

(define neg-pos-mul/prim
  (monotone2d/prim '* negative-interval nonnegative-interval nonpositive-interval
                   fl*
                   #t #f fl*/rndd fl*/rndu
                   #t #t fl//rndd fl//rndu
                   #f #t fl//rndd fl//rndu))

(define neg-neg-mul/prim
  (monotone2d/prim '* negative-interval negative-interval positive-interval
                   fl*
                   #f #f fl*/rndd fl*/rndu
                   #f #f fl//rndd fl//rndu
                   #f #f fl//rndd fl//rndu))

(: recip-fl//rndd (Flonum Flonum -> Flonum))
(define (recip-fl//rndd z x) (fl//rndd x z))

(: recip-fl//rndu (Flonum Flonum -> Flonum))
(define (recip-fl//rndu z x) (fl//rndu x z))

(define pos-pos-div/prim
  (monotone2d/prim '/ positive-interval positive-interval positive-interval
                   fl/
                   #t #f fl//rndd fl//rndu
                   #t #t fl*/rndd fl*/rndu
                   #f #t recip-fl//rndd recip-fl//rndu))

(define pos-neg-div/prim
  (monotone2d/prim '/ positive-interval negative-interval negative-interval
                   fl/
                   #f #f fl//rndd fl//rndu
                   #f #f fl*/rndd fl*/rndu
                   #f #f recip-fl//rndd recip-fl//rndu))

(define neg-pos-div/prim
  (monotone2d/prim '/ negative-interval positive-interval negative-interval
                   fl/
                   #t #t fl//rndd fl//rndu
                   #t #f fl*/rndd fl*/rndu
                   #t #f recip-fl//rndd recip-fl//rndu))

(define neg-neg-div/prim
  (monotone2d/prim '/ negative-interval negative-interval positive-interval
                   fl/
                   #f #t fl//rndd fl//rndu
                   #f #t fl*/rndd fl*/rndu
                   #t #t recip-fl//rndd recip-fl//rndu))

;; ---------------------------------------------------------------------------------------------------
;; Real predicates

(: real-predicate/prim (Symbol (Flonum -> Boolean) Nonempty-Real-Set Nonempty-Real-Set -> prim-arrow))
(define (real-predicate/prim name p? At Af)
  (predicate/prim
   name
   (λ (a) (if (flonum? a) (p? a) (bottom (delay (format "~a: expected Flonum; given ~e" a)))))
   (bot-basic At)
   (bot-basic Af)))

(define negative?/prim
  (real-predicate/prim 'negative? (λ: ([x : Flonum]) (x . fl< . 0.0))
                       negative-interval nonnegative-interval))

(define positive?/prim
  (real-predicate/prim 'positive? (λ: ([x : Flonum]) (x . fl> . 0.0))
                       positive-interval nonpositive-interval))

(define nonpositive?/prim
  (real-predicate/prim 'nonpositive? (λ: ([x : Flonum]) (x . fl<= . 0.0))
                       nonpositive-interval positive-interval))

(define nonnegative?/prim
  (real-predicate/prim 'nonnegative? (λ: ([x : Flonum]) (x . fl>= . 0.0))
                       nonnegative-interval negative-interval))

(define lt/prim (>>>/prim -/prim negative?/prim))
(define gt/prim (>>>/prim -/prim positive?/prim))
(define lte/prim (>>>/prim -/prim nonpositive?/prim))
(define gte/prim (>>>/prim -/prim nonnegative?/prim))

;; ---------------------------------------------------------------------------------------------------
;; Nonmonotone functions

;; Absolute value
(define abs/prim
  (ifte/prim negative?/prim neg/prim id/prim))

;; Square
(define sqr/prim
  (ifte/prim negative?/prim neg-sqr/prim pos-sqr/prim))

;; Reciprocal
(define recip/prim
  (ifte/prim positive?/prim
             pos-recip/prim
             (ifte/prim negative?/prim
                        neg-recip/prim
                        bottom/prim)))

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
(define partial-cos/prim
  (ifte/prim negative?/prim (>>>/prim neg/prim mono-cos/prim) mono-cos/prim))

;; Sine restricted to [-π/2,π]
(define partial-pos-sin/prim
  (ifte/prim (>>>/prim (translate/prim (* -0.5 pi)) nonpositive?/prim)
             mono-sin/prim
             (>>>/prim (translate/prim (- pi))
                       (ifte/prim nonpositive?/prim
                                  (>>>/prim neg/prim mono-sin/prim)
                                  bottom/prim))))

;; Sine restricted to [-π,π]
(define partial-sin/prim
  (ifte/prim negative?/prim
             (>>>/prim (>>>/prim neg/prim partial-pos-sin/prim) neg/prim)
             partial-pos-sin/prim))

(define real-pair (set-pair reals reals))

;; Multiplication
(define */prim
  (ifte/prim (>>>/prim (ref/prim 'fst) positive?/prim)
             (ifte/prim (>>>/prim (ref/prim 'snd) positive?/prim)
                        pos-pos-mul/prim
                        (ifte/prim (>>>/prim (ref/prim 'snd) negative?/prim)
                                   pos-neg-mul/prim
                                   (const/prim 0.0 real-pair)))
             (ifte/prim (>>>/prim (ref/prim 'fst) negative?/prim)
                        (ifte/prim (>>>/prim (ref/prim 'snd) positive?/prim)
                                   neg-pos-mul/prim
                                   (ifte/prim (>>>/prim (ref/prim 'snd) negative?/prim)
                                              neg-neg-mul/prim
                                              (const/prim 0.0 real-pair)))
                        (const/prim 0.0 real-pair))))

;; Division
(define //prim
  (ifte/prim (>>>/prim (ref/prim 'snd) positive?/prim)
             (ifte/prim (>>>/prim (ref/prim 'fst) positive?/prim)
                        pos-pos-div/prim
                        (ifte/prim (>>>/prim (ref/prim 'fst) negative?/prim)
                                   neg-pos-div/prim
                                   (const/prim 0.0 real-pair)))
             (ifte/prim (>>>/prim (ref/prim 'snd) negative?/prim)
                        (ifte/prim (>>>/prim (ref/prim 'fst) positive?/prim)
                                   pos-neg-div/prim
                                   (ifte/prim (>>>/prim (ref/prim 'fst) negative?/prim)
                                              neg-neg-div/prim
                                              (const/prim 0.0 real-pair)))
                        bottom/prim)))
