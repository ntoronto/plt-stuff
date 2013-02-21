#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/set
         math/flonum
         math/private/utils
         "types.rkt")

(provide (all-defined-out))

(define-for-syntax ((make-head-form id) stx)
  (syntax-case stx ()
    [(_ . es)  (quasisyntax/loc stx (#,id . es))]
    [_  (quasisyntax/loc stx #,id)]))

(: list-intersect (All (A) ((Listof A) (Listof A) -> (Listof A))))
(define (list-intersect xs ys)
  (let ([ys  (list->set ys)])
    (filter (λ: ([x : A]) (set-member? ys x)) xs)))

(: list-union (All (A) ((Listof A) (Listof A) -> (Listof A))))
(define (list-union xs ys)
  (remove-duplicates (append xs ys)))

(define-type Value (Rec V (U Flonum Boolean Null (Pair V V) Omega)))

(define-type Joinable-Rect (U Interval Boolean-Set Null-Rect Pair-Rect))
(define-type Nonempty-Rect (U Universal-Set Omega-Rect Join-Rect Joinable-Rect))
(define-type Rect (U Empty-Set Nonempty-Rect))

;; ===================================================================================================

(struct: Empty-Set ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (_ port write?) (fprintf port "empty-set")))

(define empty-set (Empty-Set))
(define empty-set? Empty-Set?)

(define rect-nonempty?
  (λ: ([A : Rect]) (not (empty-set? A))))

;; ===================================================================================================

(struct: Universal-Set ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (_ port write?) (fprintf port "universal-set")))

(define universal-set (Universal-Set))
(define universal-set? Universal-Set?)

;; ===================================================================================================
;; Intervals

(: print-interval (Interval Output-Port (U #t #f 0 1) -> Any))
(define (print-interval I port mode)
  (match-define (interval a b a? b?) I)
  (cond [(and a? b?)  (pretty-print-constructor 'interval (list a b) port mode)]
        [else  (pretty-print-constructor 'interval (list a b a? b?) port mode)]))

(struct: Interval ([min : Flonum] [max : Flonum] [min? : Boolean] [max? : Boolean])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-interval)

(define reals (Interval -inf.0 +inf.0 #f #f))
(define positive-reals (Interval 0.0 +inf.0 #f #f))
(define negative-reals (Interval -inf.0 -0.0 #f #f))
(define nonnegative-reals (Interval 0.0 +inf.0 #t #f))
(define nonpositive-reals (Interval -inf.0 -0.0 #f #t))
(define unit-interval (Interval 0.0 1.0 #t #t))
(define zero-reals (Interval -0.0 +0.0 #t #t))

(: make-interval (case-> (Flonum Flonum -> (U Empty-Set Interval))
                         (Flonum Flonum Boolean Boolean -> (U Empty-Set Interval))))
(define (make-interval a b [a? #t] [b? #t])
  (cond [(not (<= -inf.0 a +inf.0))  (raise-argument-error 'interval "Flonum, not NaN" 0 a b)]
        [(not (<= -inf.0 b +inf.0))  (raise-argument-error 'interval "Flonum, not NaN" 1 a b)]
        [(or (> a b) (and (= a b) (not (and a? b?))))  empty-set]
        [else
         (let ([a  (if (= a -0.0) +0.0 a)]
               [b  (if (= b +0.0) -0.0 b)])
           (cond [(fl= a -inf.0)
                  (cond [(fl= b +inf.0)  reals]
                        [else  (Interval a b #f b?)])]
                 [(fl= b +inf.0)  (Interval a b a? #f)]
                 [else  (Interval a b a? b?)]))]))

(define interval? Interval?)
(define interval-min Interval-min)
(define interval-max Interval-max)
(define interval-min? Interval-min?)
(define interval-max? Interval-max?)

(define-match-expander interval
  (λ (stx)
    (syntax-case stx ()
      [(_ a b a? b?)  (syntax/loc stx (and (? Interval?)
                                           (app Interval-min a)
                                           (app Interval-max b)
                                           (app Interval-min? a?)
                                           (app Interval-max? b?)))]))
  (make-head-form #'make-interval))

(: interval-intersect (Interval Interval -> (U Interval Empty-Set)))
(define (interval-intersect I1 I2)
  (match-define (interval a1 b1 a1? b1?) I1)
  (match-define (interval a2 b2 a2? b2?) I2)
  (define-values (a a?)
    (cond [(a1 . > . a2)  (values a1 a1?)]
          [(a1 . < . a2)  (values a2 a2?)]
          [else           (values a1 (and a1? a2?))]))
  (define-values (b b?)
    (cond [(b1 . > . b2)  (values b2 b2?)]
          [(b1 . < . b2)  (values b1 b1?)]
          [else           (values b1 (and b1? b2?))]))
  (cond [(and (fl= a a1) (fl= b b1) (eq? a? a1?) (eq? b? b1?))  I1]
        [(and (fl= a a2) (fl= b b2) (eq? a? a2?) (eq? b? b2?))  I2]
        [else  (interval a b a? b?)]))

(: interval-join (Interval Interval -> Interval))
(define (interval-join I1 I2)
  (match-define (interval a1 b1 a1? b1?) I1)
  (match-define (interval a2 b2 a2? b2?) I2)
  (define-values (a a?)
    (cond [(a1 . < . a2)  (values a1 a1?)]
          [(a1 . > . a2)  (values a2 a2?)]
          [else           (values a1 (or a1? a2?))]))
  (define-values (b b?)
    (cond [(b1 . > . b2)  (values b1 b1?)]
          [(b1 . < . b2)  (values b2 b2?)]
          [else           (values b1 (or b1? b2?))]))
  (cond [(and (fl= a a1) (fl= b b1) (eq? a? a1?) (eq? b? b1?))  I1]
        [(and (fl= a a2) (fl= b b2) (eq? a? a2?) (eq? b? b2?))  I2]
        [else  (assert (interval a b a? b?) rect-nonempty?)]))

(: interval-contains? (Interval Flonum -> Boolean))
(define (interval-contains? I x)
  (match-define (interval a b a? b?) I)
  (cond [(< a x b)  #t]
        [(< x a)  #f]
        [(< b x)  #f]
        [(and (= x a) a?)  #t]
        [(and (= x b) b?)  #t]
        [else  #f]))

(: interval-sample-point (Interval -> Flonum))
(define (interval-sample-point I)
  (match-define (interval a b a? b?) I)
  (define m (- b a))
  (define x (+ a (* m (random))))
  (cond [(and (or (not (= x a)) a?) (or (not (= x b)) b?))  x]
        [(and a? b?)  (* 0.5 (+ a b))]
        [a?  a]
        [b?  b]
        [else  (* 0.5 (+ a b))]))

(: interval-measure (Interval -> Flonum))
(define (interval-measure I)
  (- (interval-max I) (interval-min I)))

;; ===================================================================================================
;; Booleans

(define-type Boolean-Set (U 'tf 't 'f))

(define boolean-set?
  (λ: ([A : Rect]) (symbol? A)))

(: boolean-set-member? (Boolean-Set Boolean -> Boolean))
(define (boolean-set-member? A x)
  (or (eq? A 'tf)
      (and (eq? A 't) (eq? x #t))
      (and (eq? A 'f) (eq? x #f))))

(: boolean-set-join (Boolean-Set Boolean-Set -> Boolean-Set))
(define (boolean-set-join A B)
  (cond [(eq? A 'tf)  A]
        [(eq? B 'tf)  B]
        [(eq? A B)  A]
        [else  'tf]))

(: boolean-set-intersect (Boolean-Set Boolean-Set -> (U Empty-Set Boolean-Set)))
(define (boolean-set-intersect A B)
  (cond [(eq? A 'tf)  B]
        [(eq? B 'tf)  A]
        [(eq? A B)  A]
        [else  empty-set]))

(: boolean-set (case-> (-> Empty-Set)
                       (#t -> 't)
                       (#f -> 'f)
                       (#t #f -> 'tf)
                       (#f #t -> 'tf)
                       (Boolean -> Boolean-Set)
                       (Boolean Boolean -> Boolean-Set)))
(define boolean-set
  (case-lambda
    [()  empty-set]
    [(b0)  (if b0 't 'f)]
    [(b0 b1)
     (cond [b0    (if b1 't 'tf)]
           [else  (if b1 'tf 'f)])]))

(: booleans->boolean-set (case-> (#t #t -> 'tf)
                                 (#t #f -> 't)
                                 (#f #t -> 'f)
                                 (#f #f -> Empty-Set)
                                 (Boolean Boolean -> (U Empty-Set Boolean-Set))))
(define (booleans->boolean-set t? f?)
  (cond [t?    (if f? 'tf 't)]
        [else  (if f? 'f empty-set)]))

(: boolean-set->booleans (case-> ('tf -> (Values #t #t))
                                 ('t -> (Values #t #f))
                                 ('f -> (Values #f #t))
                                 (Empty-Set -> (Values #f #f))
                                 ((U Empty-Set Boolean-Set) -> (Values Boolean Boolean))))
(define (boolean-set->booleans A)
  (case A
    [(tf)  (values #t #t)]
    [(t)   (values #t #f)]
    [(f)   (values #f #t)]
    [else  (values #f #f)]))

;; ===================================================================================================
;; Null rectangle (singleton set)

(: print-null-rect (Null-Rect Output-Port (U #t #f 0 1) -> Any))
(define (print-null-rect _ port mode)
  (fprintf port "null-rect"))

(struct: Null-Rect ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-null-rect)

(define null-rect-value (Null-Rect))
(define null-rect? Null-Rect?)

(define-match-expander null-rect
  (λ (stx)
    (syntax-case stx ()
      [(_)  (syntax/loc stx (? Null-Rect?))]))
  (make-head-form #'null-rect-value))

;; ===================================================================================================
;; Pair rectangle

(: print-pair-rect (Pair-Rect Output-Port (U #t #f 0 1) -> Any))
(define (print-pair-rect p port mode)
  (pretty-print-constructor 'pair-rect (list (Pair-Rect-fst p) (Pair-Rect-snd p)) port mode))

(struct: Pair-Rect ([fst : Nonempty-Rect] [snd : Nonempty-Rect])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-pair-rect)

(define pair-rect? Pair-Rect?)
(define pair-rect-fst Pair-Rect-fst)
(define pair-rect-snd Pair-Rect-snd)

(define universal-pair (Pair-Rect universal-set universal-set))

(: make-pair-rect (case-> (Empty-Set Rect -> Empty-Set)
                          (Rect Empty-Set -> Empty-Set)
                          (Nonempty-Rect Nonempty-Rect -> Pair-Rect)
                          (Rect Rect -> (U Empty-Set Pair-Rect))))
(define (make-pair-rect A1 A2)
  (cond [(empty-set? A1)  A1]
        [(empty-set? A2)  A2]
        [(and (universal-set? A1) (universal-set? A2))  universal-pair]
        [else  (Pair-Rect A1 A2)]))

(define-match-expander pair-rect
  (λ (stx)
    (syntax-case stx ()
      [(_ A1 A2)  (syntax/loc stx
                    (and (? Pair-Rect?) (app Pair-Rect-fst A1) (app Pair-Rect-snd A2)))]))
  (make-head-form #'make-pair-rect))

(: list-rect (Rect * -> Rect))
(define (list-rect . As)
  (foldr pair-rect null-rect As))

;; ===================================================================================================
;; Infinite product space values

(define-type Omega-Hash (HashTable Omega-Idx Flonum))
(define: empty-omega-hash : Omega-Hash  (make-immutable-hash empty))

(: print-omega (Omega Output-Port (U #t #f 0 1) -> Any))
(define (print-omega ω port mode)
  (define lst (hash-map (unbox (Omega-hash ω))
                        (λ: ([k : Omega-Idx] [x : Flonum]) (cons k x))))
  (pretty-print-constructor 'omega lst port mode))

(struct: Omega ([hash : (Boxof Omega-Hash)])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-omega)

(define omega? Omega?)

(: omega (case-> (-> Omega)
                 (Omega-Hash -> Omega)))
(define (omega [h empty-omega-hash])
  (cond [(immutable? h)  (Omega (box h))]
        [else  (raise-argument-error 'omega "immutable Omega-Hash" h)]))

(: omega-hash (Omega -> Omega-Hash))
(define (omega-hash ω)
  (unbox (Omega-hash ω)))

;; ===================================================================================================
;; Infinite product space rectangles

(define-type Omega-Rect-Hash (HashTable Omega-Idx Interval))
(define: empty-omega-rect-hash : Omega-Rect-Hash  (make-immutable-hash empty))

(: print-omega-rect (Omega-Rect Output-Port (U #t #f 0 1) -> Any))
(define (print-omega-rect Ω port mode)
  (define lst (hash-map (omega-rect-hash Ω) (λ: ([k : Omega-Idx] [x : Interval]) (cons k x))))
  (pretty-print-constructor 'omega-rect lst port mode))

(struct: Omega-Rect ([hash : Omega-Rect-Hash])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-omega-rect)

(define omega-rect? Omega-Rect?)
(define omega-rect-hash Omega-Rect-hash)

(: omega-rect (case-> (-> Omega-Rect)
                      (Omega-Rect-Hash -> Omega-Rect)))
(define (omega-rect [h empty-omega-rect-hash])
  (cond [(immutable? h)  (Omega-Rect h)]
        [else  (raise-argument-error 'omega-rect "immutable Omega-Rect-Hash" h)]))

;; ===================================================================================================
;; Polymorphic join

(struct: Join-Rect ([interval : (U #f Interval)]
                    [null-rect : (U #f Null-Rect)]
                    [pair-rect : (U #f Pair-Rect)]
                    [boolean-set : (U #f Boolean-Set)])
  #:transparent)

(define join-rect? Join-Rect?)
(define join-rect-interval Join-Rect-interval)
(define join-rect-boolean-set Join-Rect-boolean-set)
(define join-rect-null-rect Join-Rect-null-rect)
(define join-rect-pair-rect Join-Rect-pair-rect)

(: make-join-rect ((U #f Empty-Set Interval)
                   (U #f Empty-Set Null-Rect)
                   (U #f Empty-Set Pair-Rect)
                   (U #f Empty-Set Boolean-Set)
                   -> Rect))
(define (make-join-rect A0 A1 A2 A3)
  (let ([A0  (if (empty-set? A0) #f A0)]
        [A1  (if (empty-set? A1) #f A1)]
        [A2  (if (empty-set? A2) #f A2)]
        [A3  (if (empty-set? A3) #f A3)])
    (define num-present (+ (if A0 1 0) (if A1 1 0) (if A2 1 0) (if A3 1 0)))
    (case num-present
      [(0)  empty-set]
      [(1)  (if A0 A0 (if A1 A1 (if A2 A2 (if A3 A3 empty-set))))]  ; empty-set won't happen
      [else
       (if (and (eq? A0 reals) A1 (eq? A2 universal-pair) (eq? A3 'tf))
           universal-set
           (Join-Rect A0 A1 A2 A3))])))

(define-match-expander join-rect
  (λ (stx)
    (syntax-case stx ()
      [(_ A0 A1 A2 A3)
       (syntax/loc stx
         (and (? Join-Rect?)
              (app Join-Rect-interval A0)
              (app Join-Rect-null-rect A1)
              (app Join-Rect-pair-rect A2)
              (app Join-Rect-boolean-set A3)))]))
  (make-head-form #'make-join-rect))

;; ===================================================================================================
;; Value operations

;; ---------------------------------------------------------------------------------------------------
;; Ref

(: value-ref (Value Idx -> Value))
(define (value-ref v j)
  (cond [(omega? v)
         (cond [(number? j)  (omega-ref v j)]
               [else  (raise-argument-error 'value-ref "Omega-Idx" 1 v j)])]
        [(and (pair? v) (or (eq? j 'fst) (eq? j 'snd) (exact-integer? j)))
         (pair-ref v j)]
        [else
         (raise 'value-ref)]))

(: pair-ref ((Pair Value Value) (U 'fst 'snd Natural) -> Value))
(define (pair-ref x1x2 j)
  (match-define (cons x1 x2) x1x2)
  (cond [(symbol? j)  (cond [(eq? j 'fst)  x1]
                            [(eq? j 'snd)  x2])]
        [(zero? j)  x1]
        [else  (value-ref x2 (- j 1))]))

(: omega-ref (Omega Omega-Idx -> Flonum))
(define (omega-ref ω r)
  (define bx (Omega-hash ω))
  (define h (unbox bx))
  (cond [(hash-has-key? h r)  (hash-ref h r)]
        [else  (define v (random))
               (set-box! bx (hash-set h r v))
               v]))

;; ---------------------------------------------------------------------------------------------------
;; Singleton

(: value->singleton (Value -> Nonempty-Rect))
(define (value->singleton v)
  (cond [(omega? v)   (omega->singleton v)]
        [(flonum? v)  (flonum->singleton v)]
        [(null? v)    null-rect]
        [(pair? v)    (pair->singleton v)]
        [else         (boolean->singleton v)]))

(: pair->singleton ((Pair Value Value) -> Pair-Rect))
(define (pair->singleton x1x2)
  (match-define (cons x1 x2) x1x2)
  (pair-rect (value->singleton x1)
             (value->singleton x2)))

(: omega->singleton (Omega -> Omega-Rect))
(define (omega->singleton ω)
  (define h (omega-hash ω))
  (omega-rect ((inst make-immutable-hash Omega-Idx Interval)
               (hash-map h (λ: ([k : Omega-Idx] [v : Flonum])
                             (cons k (flonum->singleton v)))))))

(: flonum->singleton (Flonum -> Interval))
(define (flonum->singleton x)
  (cond [(< -inf.0 x +inf.0)  (Interval x x #t #t)]
        [else  (raise-argument-error 'flonum->singleton "rational Flonum" x)]))

(: boolean->singleton (Boolean -> Boolean-Set))
(define (boolean->singleton b)
  (if b 't 'f))

;; ===================================================================================================
;; Rectangle operations

;; ---------------------------------------------------------------------------------------------------
;; Ref

(: rect-ref (case-> (Empty-Set Idx -> Empty-Set)
                    (Rect Idx -> Rect)))
(define (rect-ref A j)
  (cond [(empty-set? A)   A]
        [(omega-rect? A)
         (cond [(number? j)  (omega-rect-ref A j)]
               [else  (raise-argument-error 'rect-ref "Omega-Idx" 1 A j)])]
        [(universal-set? A)  universal-set]
        [(join-rect? A)
         (let ([A  (join-rect-pair-rect A)])
           (if A (rect-ref A j) empty-set))]
        [(and (pair-rect? A) (or (eq? j 'fst) (eq? j 'snd) (exact-integer? j)))
         (pair-rect-ref A j)]
        [else
         empty-set]))

(: pair-rect-ref (Pair-Rect (U 'fst 'snd Natural) -> Rect))
(define (pair-rect-ref A1×A2 j)
  (match-define (pair-rect A1 A2) A1×A2)
  (cond [(symbol? j)  (cond [(eq? j 'fst)  A1]
                            [(eq? j 'snd)  A2])]
        [(zero? j)  A1]
        [else  (rect-ref A2 (- j 1))]))

(: omega-rect-ref (Omega-Rect Omega-Idx -> Interval))
(define (omega-rect-ref Ω r)
  (hash-ref (omega-rect-hash Ω) r (λ () unit-interval)))

;; ---------------------------------------------------------------------------------------------------
;; Set

(: rect-set (case-> (Empty-Set Idx Rect -> Empty-Set)
                    (Rect Idx Empty-Set -> Empty-Set)
                    (Rect Idx Rect -> Rect)))
(define (rect-set A j C)
  (cond [(empty-set? A)  A]
        [(empty-set? C)  C]
        [(omega-rect? A)
         (cond [(not (number? j))    (raise-argument-error 'rect-set "Omega-Idx" 1 A j C)]
               [(not (interval? C))  (raise-argument-error 'rect-set "interval" 2 A j C)]
               [else  (omega-rect-set A j C)])]
        [(universal-set? A)
         (rect-set universal-pair j C)]
        [(join-rect? A)
         (let ([A  (join-rect-pair-rect A)])
           (if A (rect-set A j C) empty-set))]
        [(and (pair-rect? A) (or (eq? j 'fst) (eq? j 'snd) (exact-integer? j)))
         (pair-rect-set A j C)]
        [else
         empty-set]))

(: pair-rect-set (Pair-Rect (U 'fst 'snd Natural) Nonempty-Rect -> (U Empty-Set Pair-Rect)))
(define (pair-rect-set A1×A2 j C)
  (match-define (pair-rect A1 A2) A1×A2)
  (cond [(symbol? j)  (cond [(eq? j 'fst)  (if (eq? C A1) A1×A2 (pair-rect C A2))]
                            [(eq? j 'snd)  (if (eq? C A2) A1×A2 (pair-rect A1 C))])]
        [(zero? j)  (if (eq? C A1) A1×A2 (pair-rect C A2))]
        [else  (let ([C  (rect-set A2 (- j 1) C)])
                 (if (eq? C A2) A1×A2 (pair-rect A1 C)))]))

(: omega-rect-set (Omega-Rect Omega-Idx Interval -> Omega-Rect))
(define (omega-rect-set Ω r I)
  (define old-I (omega-rect-ref Ω r))
  (if (equal? I old-I) Ω (omega-rect (hash-set (omega-rect-hash Ω) r I))))

;; ---------------------------------------------------------------------------------------------------
;; Join

(: rect-join (case-> (Empty-Set Empty-Set -> Empty-Set)
                     (Rect Nonempty-Rect -> Nonempty-Rect)
                     (Nonempty-Rect Rect -> Nonempty-Rect)
                     (Rect Rect -> Rect)))
(define (rect-join A B)
  (cond [(empty-set? A)  B]
        [(empty-set? B)  A]
        [(eq? A B)  A]
        [(universal-set? A)  A]
        [(universal-set? B)  B]
        [(omega-rect? A)
         (cond [(omega-rect? B)  (omega-rect-join A B)]
               [else  (raise-argument-error 'rect-join "Omega-Rect" 1 A B)])]
        [(omega-rect? B)
         (raise-argument-error 'rect-join "Omega-Rect" 0 A B)]
        [(and (join-rect? A) (join-rect? B))  (join-join-join A B)]
        [(join-rect? A)  (join-rect-join A B)]
        [(join-rect? B)  (join-rect-join B A)]
        [(interval? A)
         (cond [(interval? B)   (interval-join A B)]
               [(null-rect? B)  (Join-Rect A B #f #f)]
               [(pair-rect? B)  (Join-Rect A #f B #f)]
               [else            (Join-Rect A #f #f B)])]
        [(null-rect? A)
         (cond [(interval? B)   (Join-Rect B A #f #f)]
               [(null-rect? B)  null-rect]
               [(pair-rect? B)  (Join-Rect #f A B #f)]
               [else            (Join-Rect #f A #f B)])]
        [(pair-rect? A)
         (cond [(interval? B)   (Join-Rect B #f A #f)]
               [(null-rect? B)  (Join-Rect #f B A #f)]
               [(pair-rect? B)  (pair-rect-join A B)]
               [else            (Join-Rect #f #f A B)])]
        [else
         (cond [(interval? B)   (Join-Rect B #f #f A)]
               [(null-rect? B)  (Join-Rect #f B #f A)]
               [(pair-rect? B)  (Join-Rect #f #f B A)]
               [else            (boolean-set-join A B)])]))

(: pair-rect-join (Pair-Rect Pair-Rect -> Pair-Rect))
(define (pair-rect-join A1×A2 B1×B2)
  (match-define (pair-rect A1 A2) A1×A2)
  (match-define (pair-rect B1 B2) B1×B2)
  (define C1 (rect-join A1 B1))
  (define C2 (rect-join A2 B2))
  (define A1? (eq? C1 A1))
  (define A2? (eq? C2 A2))
  (if (and A1? A2?)
      A1×A2
      (let ([B1?  (eq? C1 B1)]
            [B2?  (eq? C2 B2)])
        (if (and B1? B2?)
            B1×B2
            (pair-rect (if A1? A1 (if B1? B1 C1))
                       (if A2? A2 (if B2? B2 C2)))))))

(: omega-rect-join (Omega-Rect Omega-Rect -> Omega-Rect))
(define (omega-rect-join Ω1 Ω2)
  (define h1 (omega-rect-hash Ω1))
  (define h2 (omega-rect-hash Ω2))
  (define ks (list-intersect (hash-keys h1) (hash-keys h2)))
  (omega-rect
   (for/fold: ([h : Omega-Rect-Hash  empty-omega-rect-hash]) ([k  (in-list ks)])
     (hash-set h k (interval-join (hash-ref h1 k) (hash-ref h2 k))))))

(: join-join-join (Join-Rect Join-Rect -> Nonempty-Rect))
(define (join-join-join A B)
  (match-define (join-rect A0 A1 A2 A3) A)
  (match-define (join-rect B0 B1 B2 B3) B)
  (assert
   (join-rect (and A0 B0 (interval-join A0 B0))
              (and A1 B1 null-rect)
              (and A2 B2 (pair-rect-join A2 B2))
              (and A3 B3 (boolean-set-join A3 B3)))
   rect-nonempty?))

(: join-rect-join (Join-Rect Joinable-Rect -> Nonempty-Rect))
(define (join-rect-join A B)
  (match-define (join-rect A0 A1 A2 A3) A)
  (assert
   (cond [(interval? B)   (join-rect (and A0 (interval-join A0 B)) A1 A2 A3)]
         [(null-rect? B)  (join-rect A0 (and A1 null-rect) A2 A3)]
         [(pair-rect? B)  (join-rect A0 A1 (and A2 (pair-rect-join A2 B)) A3)]
         [else            (join-rect A0 A1 A2 (and A3 (boolean-set-join A3 B)))])
   rect-nonempty?))

;; ---------------------------------------------------------------------------------------------------
;; Intersect

(: rect-intersect (case-> (Empty-Set Rect -> Empty-Set)
                          (Rect Empty-Set -> Empty-Set)
                          (Rect Rect -> Rect)))
(define (rect-intersect A B)
  (cond [(empty-set? A)  A]
        [(empty-set? B)  B]
        [(eq? A B)  A]
        [(universal-set? A)  B]
        [(universal-set? B)  A]
        [(omega-rect? A)
         (cond [(omega-rect? B)  (omega-rect-intersect A B)]
               [else  (raise-argument-error 'rect-intersect "Omega-Rect" 1 A B)])]
        [(omega-rect? B)
         (raise-argument-error 'rect-intersect "Omega-Rect" 0 A B)]
        [(and (join-rect? A) (join-rect? B))  (join-join-intersect A B)]
        [(join-rect? A)  (join-rect-intersect A B)]
        [(join-rect? B)  (join-rect-intersect B A)]
        [(interval? A)
         (cond [(interval? B)  (interval-intersect A B)]
               [else  empty-set])]
        [(null-rect? A)
         (cond [(null-rect? B)  null-rect]
               [else  empty-set])]
        [(pair-rect? A)
         (cond [(pair-rect? B)  (pair-rect-intersect A B)]
               [else  empty-set])]
        [else
         (cond [(boolean-set? B)  (boolean-set-intersect A B)]
               [else  empty-set])]))

(: pair-rect-intersect (Pair-Rect Pair-Rect -> (U Empty-Set Pair-Rect)))
(define (pair-rect-intersect A1×A2 B1×B2)
  (match-define (pair-rect A1 A2) A1×A2)
  (match-define (pair-rect B1 B2) B1×B2)
  (define C1 (rect-intersect A1 B1))
  (define C2 (rect-intersect A2 B2))
  (define A1? (eq? C1 A1))
  (define A2? (eq? C2 A2))
  (if (and A1? A2?)
      A1×A2
      (let ([B1?  (eq? C1 B1)]
            [B2?  (eq? C2 B2)])
        (if (and B1? B2?)
            B1×B2
            (pair-rect (if A1? A1 (if B1? B1 C1))
                       (if A2? A2 (if B2? B2 C2)))))))

(: omega-rect-intersect (Omega-Rect Omega-Rect -> (U Empty-Set Omega-Rect)))
(define (omega-rect-intersect Ω1 Ω2)
  (define h1 (omega-rect-hash Ω1))
  (define h2 (omega-rect-hash Ω2))
  (let-values ([(h1 h2)  (if ((hash-count h2) . < . (hash-count h1))
                             (values h2 h1)
                             (values h1 h2))])
    (let: loop ([ks  (hash-keys h2)] [h : Omega-Rect-Hash  h1])
      (cond [(empty? ks)  (omega-rect h)]
            [else  (define k (first ks))
                   (define A (hash-ref h k (λ () #f)))
                   (define B (hash-ref h2 k))
                   (cond [A
                          (define C (interval-intersect A B))
                          (cond [(empty-set? C)  empty-set]
                                [else
                                 (loop (rest ks)
                                       (cond [(eq? C A)  h]
                                             ;; This annotation is friggin' ridiculous
                                             [else  ((inst hash-set Omega-Idx Interval) h k C)]))])]
                         [else
                          (loop (rest ks) (hash-set h k B))])]))))

(: join-join-intersect (Join-Rect Join-Rect -> Rect))
(define (join-join-intersect A B)
  (match-define (join-rect A0 A1 A2 A3) A)
  (match-define (join-rect B0 B1 B2 B3) B)
  (join-rect (and A0 B0 (interval-intersect A0 B0))
             (and A1 B1 null-rect)
             (and A2 B2 (pair-rect-intersect A2 B2))
             (and A3 B3 (boolean-set-intersect A3 B3))))

(: join-rect-intersect (Join-Rect Joinable-Rect -> Rect))
(define (join-rect-intersect A B)
  (cond [(interval? B)   (let ([A  (join-rect-interval A)])
                           (if A (interval-intersect A B) empty-set))]
        [(null-rect? B)  (let ([A  (join-rect-null-rect A)])
                           (if A null-rect empty-set))]
        [(pair-rect? B)  (let ([A  (join-rect-pair-rect A)])
                           (if A (pair-rect-intersect A B) empty-set))]
        [else            (let ([A  (join-rect-boolean-set A)])
                           (if A (boolean-set-intersect A B) empty-set))]))

(: rect-disjoint2? (Rect Rect -> Boolean))
(define (rect-disjoint2? A B)
  (empty-set? (rect-intersect A B)))

(: rect-disjoint? (Rect * -> Boolean))
(define (rect-disjoint? . As)
  (let loop0 ([As As])
    (if (empty? As)
        #t
        (let-values ([(A As)  (values (first As) (rest As))])
          (let loop1 ([Bs As])
            (if (empty? Bs)
                (loop0 As)
                (and (rect-disjoint2? A (first Bs)) (loop1 (rest Bs)))))))))

;; ---------------------------------------------------------------------------------------------------
;; Membership

(: rect-member? (case-> (Empty-Set Value -> #f)
                        (Rect Value -> Boolean)))
(define (rect-member? A x)
  (cond [(empty-set? A)  #f]
        [(universal-set? A)  #t]
        [(omega-rect? A)
         (cond [(omega? x)  (omega-rect-member? A x)]
               [else  (raise-argument-error 'rect-member? "Omega" 1 A x)])]
        [(join-rect? A)  (join-rect-member? A x)]
        [(interval? A)   (and (flonum? x) (interval-contains? A x))]
        [(null-rect? A)  (null? x)]
        [(pair-rect? A)  (and (pair? x) (pair-rect-member? A x))]
        [else            (and (boolean? x) (boolean-set-member? A x))]))

(: pair-rect-member? (Pair-Rect (Pair Value Value) -> Boolean))
(define (pair-rect-member? A1×A2 x1x2)
  (match-define (pair-rect A1 A2) A1×A2)
  (and (rect-member? A1 (car x1x2))
       (rect-member? A2 (cdr x1x2))))

(: omega-rect-member? (Omega-Rect Omega -> Boolean))
(define (omega-rect-member? Ω ω)
  (define h1 (omega-rect-hash Ω))
  (define h2 (omega-hash ω))
  (define ks (list-union (hash-keys h1) (hash-keys h2)))
  (andmap (λ: ([k : Omega-Idx])
            (interval-contains? (hash-ref h1 k (λ () unit-interval))
                                (omega-ref ω k)))
          ks))

(: join-rect-member? (Join-Rect Value -> Boolean))
(define (join-rect-member? A x)
  (match-define (join-rect A0 A1 A2 A3) A)
  (or (and A0 (rect-member? A0 x))
      (and A1 (rect-member? A1 x))
      (and A2 (rect-member? A2 x))
      (and A3 (rect-member? A3 x))))

;; ===================================================================================================
;; Branches rectangles
;; Sets of Omega-Idx -> Boolean-Set, representing the branches taken in a program

(define-type Branches-Rect (HashTable Omega-Idx (U 't 'f)))

(define: branches-rect : Branches-Rect  (make-immutable-hash empty))

(: branches-rect-set (case-> (Branches-Rect Omega-Idx Boolean-Set -> Branches-Rect)
                             (Branches-Rect Omega-Idx (U Empty-Set Boolean-Set) ->
                                            (U Empty-Set Branches-Rect))))
(define (branches-rect-set bs r b)
  (cond [(empty-set? b)  empty-set]
        [(eq? b 'tf)  (hash-remove bs r)]
        [else
         (define old-b (hash-ref bs r (λ () #f)))
         (cond [(eq? old-b b)  bs]
               [else  (hash-set bs r b)])]))

(: branches-rect-ref (Branches-Rect Omega-Idx -> Boolean-Set))
(define (branches-rect-ref bs r)
  (hash-ref bs r (λ () 'tf)))

(: branches-rect-intersect (Branches-Rect Branches-Rect -> (U Empty-Set Branches-Rect)))
(define (branches-rect-intersect bs1 bs2)
  (let-values ([(bs1 bs2)  (if ((hash-count bs2) . < . (hash-count bs1))
                               (values bs2 bs1)
                               (values bs1 bs2))])
    (let: loop ([ks  (hash-keys bs2)] [bs : Branches-Rect  bs1])
      (cond [(empty? ks)  bs]
            [else  (define k (first ks))
                   (define A (hash-ref bs k (λ () #f)))
                   (define B (hash-ref bs2 k))
                   (cond [(eq? A B)  (loop (rest ks) bs)]
                         [A  empty-set]
                         [else  (loop (rest ks) (hash-set bs k B))])]))))

(: branches-rect-restrict (Branches-Rect Omega-Idx (U 't 'f) -> (U Empty-Set Branches-Rect)))
(define (branches-rect-restrict bs r B)
  (define old-B (hash-ref bs r (λ () #f)))
  (cond [(eq? old-B B)  bs]
        [B  empty-set]
        [else  (hash-set bs r B)]))

;; ===================================================================================================
;; Extra Omega ops

(: omega-domain (Omega -> (Listof Omega-Idx)))
(define (omega-domain ω)
  (sort (hash-keys (omega-hash ω)) <))

(: omega-rect-domain (Omega-Rect -> (Listof Omega-Idx)))
(define (omega-rect-domain Ω)
  (sort (hash-keys (omega-rect-hash Ω)) <))

(: omega-rect-sample-point (Omega-Rect -> Omega))
(define (omega-rect-sample-point Ω)
  (define h (omega-rect-hash Ω))
  (omega ((inst make-immutable-hash Omega-Idx Flonum)
          (hash-map h (λ: ([k : Omega-Idx] [A : Interval])
                        (cons k (interval-sample-point A)))))))

(: omega-rect-measure (Omega-Rect -> Flonum))
(define (omega-rect-measure Ω)
  (define h (omega-rect-hash Ω))
  (fl (apply * (hash-map h (λ: ([k : Omega-Idx] [A : Interval])
                             (interval-measure A))))))
