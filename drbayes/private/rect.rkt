#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/set
         math/flonum
         math/private/utils
         "omega.rkt")

(provide (all-defined-out))

(define-for-syntax ((make-head-form id) stx)
  (syntax-case stx ()
    [(_ . es)  (quasisyntax/loc stx (#,id . es))]
    [_  (quasisyntax/loc stx #,id)]))

(define-type Value (Rec V (U Flonum Boolean Null (Pair V V))))

(define-type Joinable-Rect (U Interval Boolean-Set Null-Rect Pair-Rect))
(define-type Nonempty-Rect (U Universal-Set Join-Rect Joinable-Rect))
(define-type Rect (U Empty-Set Nonempty-Rect))

;; ===================================================================================================

(struct: Empty-Set ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (_ port write?) (fprintf port "empty-set")))

(define empty-set (Empty-Set))
(define-syntax empty-set? (make-rename-transformer #'Empty-Set?))

(define rect-nonempty?
  (λ: ([A : Rect]) (not (empty-set? A))))

;; ===================================================================================================

(struct: Universal-Set ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (_ port write?) (fprintf port "universal-set")))

(define universal-set (Universal-Set))
(define-syntax universal-set? (make-rename-transformer #'Universal-Set?))

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

(define-syntax interval? (make-rename-transformer #'Interval?))
(define-syntax interval-min (make-rename-transformer #'Interval-min))
(define-syntax interval-max (make-rename-transformer #'Interval-max))
(define-syntax interval-min? (make-rename-transformer #'Interval-min?))
(define-syntax interval-max? (make-rename-transformer #'Interval-max?))

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

(: interval-subseteq? (Interval Interval -> Boolean))
(define (interval-subseteq? I1 I2)
  (match-define (interval a1 b1 a1? b1?) I1)
  (match-define (interval a2 b2 a2? b2?) I2)
  (and (or (a1 . > . a2) (and (= a1 a2) (or (not a1?) a2?)))
       (or (b1 . < . b2) (and (= b1 b2) (or (not b1?) b2?)))))

(: interval-member? (Interval Flonum -> Boolean))
(define (interval-member? I x)
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

(: boolean-set-subseteq? (Boolean-Set Boolean-Set -> Boolean))
(define (boolean-set-subseteq? A B)
  (cond [(eq? B 'tf)  #t]
        [(eq? A B)  #t]
        [else  #f]))

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
(define-syntax null-rect? (make-rename-transformer #'Null-Rect?))

(define-match-expander null-rect
  (λ (stx)
    (syntax-case stx ()
      [(_)  (syntax/loc stx (? Null-Rect?))]))
  (make-head-form #'null-rect-value))

;; ===================================================================================================
;; Pair rectangle

(define-type Idx (U Symbol Natural))

(: print-pair-rect (Pair-Rect Output-Port (U #t #f 0 1) -> Any))
(define (print-pair-rect p port mode)
  (pretty-print-constructor 'pair-rect (list (Pair-Rect-fst p) (Pair-Rect-snd p)) port mode))

(struct: Pair-Rect ([fst : Nonempty-Rect] [snd : Nonempty-Rect])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-pair-rect)

(define-syntax pair-rect? (make-rename-transformer #'Pair-Rect?))
(define-syntax pair-rect-fst (make-rename-transformer #'Pair-Rect-fst))
(define-syntax pair-rect-snd (make-rename-transformer #'Pair-Rect-snd))

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

(: list*-rect (Rect Rect * -> Rect))
(define (list*-rect A . As)
  (let loop ([A A] [As As])
    (cond [(empty? As)  A]
          [else  (pair-rect A (loop (first As) (rest As)))])))

;; ===================================================================================================
;; Polymorphic join

(struct: Join-Rect ([interval : (U #f Interval)]
                    [null-rect : (U #f Null-Rect)]
                    [pair-rect : (U #f Pair-Rect)]
                    [boolean-set : (U #f Boolean-Set)])
  #:transparent)

(define-syntax join-rect? (make-rename-transformer #'Join-Rect?))
(define-syntax join-rect-interval (make-rename-transformer #'Join-Rect-interval))
(define-syntax join-rect-boolean-set (make-rename-transformer #'Join-Rect-boolean-set))
(define-syntax join-rect-null-rect (make-rename-transformer #'Join-Rect-null-rect))
(define-syntax join-rect-pair-rect (make-rename-transformer #'Join-Rect-pair-rect))

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
       (if (and (equal? A0 reals) A1 (eq? A2 universal-pair) (eq? A3 'tf))
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
  (cond [(not (pair? v))
         (raise-argument-error 'value-ref "Pair" 0 v j)]
        [(not (or (eq? j 'fst) (eq? j 'snd) (exact-integer? j)))
         (raise-argument-error 'value-ref "(U 'fst 'snd Natural)" 1 v j)]
        [else
         (pair-ref v j)]))

(: pair-ref ((Pair Value Value) (U 'fst 'snd Natural) -> Value))
(define (pair-ref x1x2 j)
  (match-define (cons x1 x2) x1x2)
  (cond [(symbol? j)  (cond [(eq? j 'fst)  x1]
                            [(eq? j 'snd)  x2])]
        [(zero? j)  x1]
        [else  (value-ref x2 (- j 1))]))

;; ---------------------------------------------------------------------------------------------------
;; Singleton

(: value->singleton (Value -> Nonempty-Rect))
(define (value->singleton v)
  (cond [(flonum? v)  (flonum->singleton v)]
        [(null? v)    null-rect]
        [(pair? v)    (pair->singleton v)]
        [else         (boolean->singleton v)]))

(: pair->singleton ((Pair Value Value) -> Pair-Rect))
(define (pair->singleton x1x2)
  (match-define (cons x1 x2) x1x2)
  (pair-rect (value->singleton x1)
             (value->singleton x2)))

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

;; ---------------------------------------------------------------------------------------------------
;; Set

(: rect-set (case-> (Empty-Set Idx Rect -> Empty-Set)
                    (Rect Idx Empty-Set -> Empty-Set)
                    (Rect Idx Rect -> Rect)))
(define (rect-set A j C)
  (cond [(empty-set? A)  A]
        [(empty-set? C)  C]
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

(: join-join-join (Join-Rect Join-Rect -> Nonempty-Rect))
(define (join-join-join A B)
  (match-define (join-rect A0 A1 A2 A3) A)
  (match-define (join-rect B0 B1 B2 B3) B)
  (assert
   (join-rect (if (and A0 B0) (interval-join A0 B0) (if A0 A0 B0))
              (if A1 A1 B1)
              (if (and A2 B2) (pair-rect-join A2 B2) (if A2 A2 B2))
              (if (and A3 B3) (boolean-set-join A3 B3) (if A3 A3 B3)))
   rect-nonempty?))

(: join-rect-join (Join-Rect Joinable-Rect -> Nonempty-Rect))
(define (join-rect-join A B)
  (match-define (join-rect A0 A1 A2 A3) A)
  (assert
   (cond [(interval? B)   (join-rect (if A0 (interval-join A0 B) B) A1 A2 A3)]
         [(null-rect? B)  (join-rect A0 B A2 A3)]
         [(pair-rect? B)  (join-rect A0 A1 (if A2 (pair-rect-join A2 B) B) A3)]
         [else            (join-rect A0 A1 A2 (if A3 (boolean-set-join A3 B) B))])
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
  (define A1 (pair-rect-fst A1×A2))
  (define A2 (pair-rect-snd A1×A2))
  (define B1 (pair-rect-fst B1×B2))
  (define B2 (pair-rect-snd B1×B2))
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
;; Subset or equal

(: rect-subseteq? (Rect Rect -> Boolean))
(define (rect-subseteq? A B)
  (cond [(empty-set? A)  #t]
        [(empty-set? B)  #f]
        [(universal-set? B)  #t]
        [(universal-set? A)  #f]
        [(eq? A B)  #t]
        [(and (join-rect? A) (join-rect? B))  (join-join-subseteq? A B)]
        [(join-rect? B)  (rect-join-subseteq? A B)]
        [(join-rect? A)  #f]
        [(and (interval? A) (interval? B))  (interval-subseteq? A B)]
        [(and (null-rect? A) (null-rect? B))  #t]
        [(and (pair-rect? A) (pair-rect? B))  (pair-rect-subseteq? A B)]
        [(and (boolean-set? A) (boolean-set? B))  (boolean-set-subseteq? A B)]
        [else  #f]))

(: pair-rect-subseteq? (Pair-Rect Pair-Rect -> Boolean))
(define (pair-rect-subseteq? A B)
  (and (rect-subseteq? (pair-rect-fst A) (pair-rect-fst B))
       (rect-subseteq? (pair-rect-snd A) (pair-rect-snd B))))

(: maybe-rect->rect ((U #f Nonempty-Rect) -> Rect))
(define (maybe-rect->rect A) (if A A empty-set))

(: join-join-subseteq? (Join-Rect Join-Rect -> Boolean))
(define (join-join-subseteq? A B)
  (match-define (join-rect (app maybe-rect->rect A0)
                           (app maybe-rect->rect A1)
                           (app maybe-rect->rect A2)
                           (app maybe-rect->rect A3))
    A)
  (match-define (join-rect (app maybe-rect->rect B0)
                           (app maybe-rect->rect B1)
                           (app maybe-rect->rect B2)
                           (app maybe-rect->rect B3))
    B)
  (and (rect-subseteq? A0 B0)
       (rect-subseteq? A1 B1)
       (rect-subseteq? A2 B2)
       (rect-subseteq? A3 B3)))

(: rect-join-subseteq? (Joinable-Rect Join-Rect -> Boolean))
(define (rect-join-subseteq? A B)
  (cond [(interval? A)  (rect-subseteq? A (maybe-rect->rect (join-rect-interval B)))]
        [(null-rect? A)  (null-rect? (join-rect-null-rect B))]
        [(pair-rect? A)  (rect-subseteq? A (maybe-rect->rect (join-rect-pair-rect B)))]
        [else  (rect-subseteq? A (maybe-rect->rect (join-rect-boolean-set B)))]))

;; ---------------------------------------------------------------------------------------------------
;; Membership

(: rect-member? (case-> (Empty-Set Value -> #f)
                        (Rect Value -> Boolean)))
(define (rect-member? A x)
  (cond [(empty-set? A)  #f]
        [(universal-set? A)  #t]
        [(join-rect? A)  (join-rect-member? A x)]
        [(interval? A)   (and (flonum? x) (interval-member? A x))]
        [(null-rect? A)  (null? x)]
        [(pair-rect? A)  (and (pair? x) (pair-rect-member? A x))]
        [else            (and (boolean? x) (boolean-set-member? A x))]))

(: pair-rect-member? (Pair-Rect (Pair Value Value) -> Boolean))
(define (pair-rect-member? A1×A2 x1x2)
  (match-define (pair-rect A1 A2) A1×A2)
  (and (rect-member? A1 (car x1x2))
       (rect-member? A2 (cdr x1x2))))

(: join-rect-member? (Join-Rect Value -> Boolean))
(define (join-rect-member? A x)
  (match-define (join-rect A0 A1 A2 A3) A)
  (or (and A0 (rect-member? A0 x))
      (and A1 (rect-member? A1 x))
      (and A2 (rect-member? A2 x))
      (and A3 (rect-member? A3 x))))

;; ===================================================================================================
;; Infinite product space values

(define-type Omega-Hash (Omega-Tree Flonum))

(struct: Omega ([hash : (Boxof Omega-Hash)])
  #:transparent)

(define-syntax omega? (make-rename-transformer #'Omega?))
(define-syntax omega-hash (make-rename-transformer #'Omega-hash))

(define omega-hash-default +nan.0)
(define omega-hash-ref ((inst omega-tree-ref Flonum) omega-hash-default))
(define omega-hash-set ((inst omega-tree-set Flonum) omega-hash-default))

(: omega-ref (Omega Omega-Idx -> Flonum))
(define (omega-ref ω k)
  (define h (omega-hash ω))
  (define x (omega-hash-ref (unbox h) k))
  (cond [(rational? x)  x]
        [else  (define x (random))
               (set-box! h (omega-hash-set (unbox h) k x))
               x]))

(: omega-map (All (B) (Omega (Flonum -> B) -> (Listof B))))
(define (omega-map ω f)
  (((inst omega-tree-map Flonum B) omega-hash-default) (unbox (Omega-hash ω)) f))

;; ===================================================================================================
;; Infinite product space rectangles

(define-type Omega-Rect (Omega-Tree Interval))

(define omega-nonempty?
  (λ: ([Ω : (U Empty-Set Omega-Rect)]) (not (empty-set? Ω))))

(define omega-rect-ref ((inst omega-tree-ref Interval) unit-interval))
(define omega-rect-set ((inst omega-tree-set Interval) unit-interval))

(: omega-rect-fst (Omega-Rect -> Omega-Rect))
(define omega-rect-fst omega-tree-fst)

(: omega-rect-snd (Omega-Rect -> Omega-Rect))
(define omega-rect-snd omega-tree-snd)

(: omega-rect-value (Omega-Rect -> Interval))
(define omega-rect-value (omega-tree-value unit-interval))

(: omega-rect Omega-Rect)
(define omega-rect omega-leaf)

(: omega-rect-map (All (B) (Omega-Rect (Interval -> B) -> (Listof B))))
(define (omega-rect-map Ω f)
  (((inst omega-tree-map Interval B) unit-interval) Ω f))

(define just-omega-rect-node ((inst omega-node Interval) unit-interval))

(define just-omega-rect-join
  ((inst omega-tree-join Interval) unit-interval interval-join))

(define just-omega-rect-intersect
  ((inst omega-tree-intersect Interval Empty-Set) unit-interval interval-intersect empty-set?))

(: omega-rect-node
   (Interval (U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect) -> (U Empty-Set Omega-Rect)))
(define (omega-rect-node I Ω1 Ω2)
  (cond [(empty-set? Ω1)  Ω1]
        [(empty-set? Ω2)  Ω2]
        [else  (just-omega-rect-node I Ω1 Ω2)]))

(: omega-rect-node/last
   (Omega-Rect Interval (U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect)
               -> (U Empty-Set Omega-Rect)))
(define (omega-rect-node/last Ω I Ω1 Ω2)
  (cond [(and (equal? I (omega-rect-value Ω))
              (eq? Ω1 (omega-rect-fst Ω))
              (eq? Ω2 (omega-rect-snd Ω)))
         Ω]
        [else  (unit-omega-rect-node Ω1 Ω2)]))

(: unit-omega-rect-node
   ((U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect) -> (U Empty-Set Omega-Rect)))
(define (unit-omega-rect-node Ω1 Ω2)
  (omega-rect-node unit-interval Ω1 Ω2))

(: unit-omega-rect-node/last
   (Omega-Rect (U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect) -> (U Empty-Set Omega-Rect)))
(define (unit-omega-rect-node/last Ω Ω1 Ω2)
  (cond [(and (eq? Ω1 (omega-rect-fst Ω)) (eq? Ω2 (omega-rect-snd Ω)))  Ω]
        [else  (unit-omega-rect-node Ω1 Ω2)]))

(: omega-rect-join
   (case-> (Empty-Set Empty-Set -> Empty-Set)
           ((U Empty-Set Omega-Rect) Omega-Rect -> Omega-Rect)
           (Omega-Rect (U Empty-Set Omega-Rect) -> Omega-Rect)
           ((U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect) -> (U Empty-Set Omega-Rect))))
(define (omega-rect-join Ω1 Ω2)
  (cond [(empty-set? Ω1)  Ω2]
        [(empty-set? Ω2)  Ω1]
        [else  (just-omega-rect-join Ω1 Ω2)]))

(: omega-rect-intersect
   (case-> (Empty-Set (U Empty-Set Omega-Rect) -> Empty-Set)
           ((U Empty-Set Omega-Rect) Empty-Set -> Empty-Set)
           ((U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect) -> (U Empty-Set Omega-Rect))))
(define (omega-rect-intersect Ω1 Ω2)
  (cond [(empty-set? Ω1)  Ω1]
        [(empty-set? Ω2)  Ω2]
        [else  (just-omega-rect-intersect Ω1 Ω2)]))

(define omega-rect->omega-hash
  ((inst omega-tree->omega-tree Interval Flonum) unit-interval omega-hash-default))

(: omega-rect-sample-point (Omega-Rect -> Omega))
(define (omega-rect-sample-point Ω)
  (Omega (box (omega-rect->omega-hash Ω interval-sample-point))))

(: omega-rect-measure (Omega-Rect -> Flonum))
(define (omega-rect-measure Ω)
  (fl (apply * (omega-rect-map Ω interval-measure))))

;; ===================================================================================================
;; Conditional bisection rectangles

(define-type Branches-Rect (Omega-Tree Boolean-Set))

(define branches-rect-ref ((inst omega-tree-ref Boolean-Set) 'tf))
(define branches-rect-set ((inst omega-tree-set Boolean-Set) 'tf))

(: branches-rect-fst (Branches-Rect -> Branches-Rect))
(define branches-rect-fst omega-tree-fst)

(: branches-rect-snd (Branches-Rect -> Branches-Rect))
(define branches-rect-snd omega-tree-snd)

(: branches-rect-value (Branches-Rect -> Boolean-Set))
(define branches-rect-value (omega-tree-value 'tf))

(: branches-rect Branches-Rect)
(define branches-rect omega-leaf)

(define branches-rect-node ((inst omega-node Boolean-Set) 'tf))

(: branches-rect-node/last
   (Branches-Rect Boolean-Set Branches-Rect Branches-Rect -> Branches-Rect))
(define (branches-rect-node/last Z b Z1 Z2)
  (cond [(and (eq? b (branches-rect-value Z))
              (eq? Z1 (branches-rect-fst Z))
              (eq? Z2 (branches-rect-snd Z)))
         Z]
        [else  (branches-rect-node b Z1 Z2)]))
