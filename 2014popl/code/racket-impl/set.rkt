#lang typed/racket/base

(require "../types.rkt"
         "../untyped-utils.rkt"
         racket/match)

(provide (all-defined-out))

;; class set S X where ...
(struct: (S X) set ([empty : S]
                    [univ : S]
                    [meet : (S S -> S)]
                    [join : (S S -> S)]
                    [member? : (S X -> Boolean)]
                    [singleton : (X -> S)]))

(: set-empty? (All (S X) ((set S X) -> (S -> Boolean))))
(define ((set-empty? ops) A)
  (equal? (set-empty ops) A))

(: set-univ? (All (S X) ((set S X) -> (S -> Boolean))))
(define ((set-univ? ops) A)
  (equal? (set-univ ops) A))

;; ---------------------------------------------------------------------------------------------------
;; Reals

(struct: interval ([min : Flonum] [max : Flonum]) #:transparent)

(define-singleton-type Empty-Real-Set empty-real-set)
(define-singleton-type Univ-Real-Set univ-real-set)
(define-type Real-Set (U Empty-Real-Set Univ-Real-Set interval))

(: ivl (Flonum Flonum -> Real-Set))
(define (ivl min max)
  (if (min . <= . max) (interval min max) empty-real-set))

(: real-set-meet (Real-Set Real-Set -> Real-Set))
(define (real-set-meet A B)
  (cond [(or (empty-real-set? A) (empty-real-set? B))  empty-real-set]
        [(univ-real-set? A)  B]
        [(univ-real-set? B)  A]
        [else  (match-define (interval amin amax) A)
               (match-define (interval bmin bmax) B)
               (ivl (max amin bmin) (min amax bmax))]))

(: real-set-join (Real-Set Real-Set -> Real-Set))
(define (real-set-join A B)
  (cond [(or (univ-real-set? A) (univ-real-set? B))  univ-real-set]
        [(empty-real-set? A)  B]
        [(empty-real-set? B)  A]
        [else  (match-define (interval amin amax) A)
               (match-define (interval bmin bmax) B)
               (interval (min amin bmin) (max amax bmax))]))

(: real-set-member? (Real-Set Flonum -> Boolean))
(define (real-set-member? A a)
  (cond [(empty-real-set? A)  #f]
        [(univ-real-set? A)   #t]
        [else  (match-define (interval amin amax) A)
               (<= amin a amax)]))

(: real-set-singleton (Flonum -> Real-Set))
(define (real-set-singleton a)
  (interval a a))

;; instance set Real-Set Flonum where ...
(: real-set-ops (set Real-Set Flonum))
(define real-set-ops
  (set empty-real-set univ-real-set real-set-meet real-set-join real-set-member? real-set-singleton))

;; ---------------------------------------------------------------------------------------------------
;; Booleans

(define-singleton-type Empty-Bool-Set empty-bool-set)
(define-singleton-type Univ-Bool-Set univ-bool-set)
(define-type Bool-Set (U Empty-Bool-Set Univ-Bool-Set #t #f))

(: bool-set-meet (Bool-Set Bool-Set -> Bool-Set))
(define (bool-set-meet A B)
  (cond [(or (empty-bool-set? A) (empty-bool-set? B))  empty-bool-set]
        [(univ-bool-set? A)  B]
        [(univ-bool-set? B)  A]
        [(eq? A B)  A]
        [else  empty-bool-set]))

(: bool-set-join (Bool-Set Bool-Set -> Bool-Set))
(define (bool-set-join A B)
  (cond [(or (univ-bool-set? A) (univ-bool-set? B))  univ-bool-set]
        [(empty-bool-set? A)  B]
        [(empty-bool-set? B)  A]
        [(eq? A B)  A]
        [else  univ-bool-set]))

(: bool-set-member? (Bool-Set Boolean -> Boolean))
(define (bool-set-member? A a)
  (cond [(empty-bool-set? A)  #f]
        [(univ-bool-set? A)   #t]
        [else  (eq? A a)]))

(: bool-set-singleton (Boolean -> Bool-Set))
(define (bool-set-singleton a) a)

;; instance set Bool-Set Boolean where ...
(: bool-set-ops (set Bool-Set Boolean))
(define bool-set-ops
  (set empty-bool-set univ-bool-set bool-set-meet bool-set-join bool-set-member? bool-set-singleton))

;; ---------------------------------------------------------------------------------------------------
;; Pairs

(struct: (S1 S2) pair-set ([fst : S1] [snd : S2]) #:transparent)

(define-singleton-type Empty-Pair-Set empty-pair-set)
(define-singleton-type Univ-Pair-Set univ-pair-set)
(define-type (Pair-Set S1 S2) (U Empty-Pair-Set Univ-Pair-Set (pair-set S1 S2)))

;; set-prod :: (set S1 X1, set S2 X2) => S1 -> S2 -> Pair-Set S1 S2
(: set-prod (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (S1 S2 -> (Pair-Set S1 S2)))))
(define (set-prod ops1 ops2)
  (define empty1? (set-empty? ops1))
  (define empty2? (set-empty? ops2))
  (define univ1? (set-univ? ops1))
  (define univ2? (set-univ? ops2))
  (λ (A1 A2)
    (cond [(or (empty1? A1) (empty2? A2))  empty-pair-set]
          [(and (univ1? A1) (univ2? A2))   univ-pair-set]
          [else  (pair-set A1 A2)])))

(: set-proj-fst (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> ((Pair-Set S1 S2) -> S1))))
(define (set-proj-fst ops1 ops2)
  (define empty1 (set-empty ops1))
  (define univ1 (set-univ ops1))
  (λ (A)
    (cond [(empty-pair-set? A)  empty1]
          [(univ-pair-set? A)   univ1]
          [else  (pair-set-fst A)])))

(: set-proj-snd (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> ((Pair-Set S1 S2) -> S2))))
(define (set-proj-snd ops1 ops2)
  (define empty2 (set-empty ops2))
  (define univ2 (set-univ ops2))
  (λ (A)
    (cond [(empty-pair-set? A)  empty2]
          [(univ-pair-set? A)   univ2]
          [else  (pair-set-snd A)])))

;; instance (set S1 X1, set S2 X2) => set (Pair-Set S1 S2) (Pair X1 X2) where ...
(: pair-set-ops (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (set (Pair-Set S1 S2) (Pair X1 X2)))))
(define (pair-set-ops ops1 ops2)
  (define prod (set-prod ops1 ops2))
  (match-define (set _empty1 _univ1 meet1 join1 member1? singleton1) ops1)
  (match-define (set _empty2 _univ2 meet2 join2 member2? singleton2) ops2)
  (set empty-pair-set
       univ-pair-set
       ;; meet
       (λ: ([A : (Pair-Set S1 S2)] [B : (Pair-Set S1 S2)])
         (cond [(or (empty-pair-set? A) (empty-pair-set? B))  empty-pair-set]
               [(univ-pair-set? A)  B]
               [(univ-pair-set? B)  A]
               [else  (prod (meet1 (pair-set-fst A) (pair-set-fst B))
                            (meet2 (pair-set-snd A) (pair-set-snd B)))]))
       ;; join
       (λ: ([A : (Pair-Set S1 S2)] [B : (Pair-Set S1 S2)])
         (cond [(or (univ-pair-set? A) (univ-pair-set? B))  univ-pair-set]
               [(empty-pair-set? A)  B]
               [(empty-pair-set? B)  A]
               [else  (prod (join1 (pair-set-fst A) (pair-set-fst B))
                            (join2 (pair-set-snd A) (pair-set-snd B)))]))
       ;; member?
       (λ: ([A : (Pair-Set S1 S2)] [a : (Pair X1 X2)])
         (cond [(empty-pair-set? A)  #f]
               [(univ-pair-set? A)   #t]
               [else  (and (member1? (pair-set-fst A) (car a))
                           (member2? (pair-set-snd A) (cdr a)))]))
       ;; singleton
       (λ: ([a : (Pair X1 X2)])
         (pair-set (singleton1 (car a))
                   (singleton2 (cdr a))))))

;; ---------------------------------------------------------------------------------------------------
;; Sets of singleton types (e.g. Null or 'bob)

(struct: (X) empty-singleton-set ([value : X]) #:transparent)
(struct: (X) univ-singleton-set ([value : X]) #:transparent)
(define-type (Singleton-Set X) (U (empty-singleton-set X) (univ-singleton-set X)))

(: singleton-set-ops (All (X) (X -> (set (Singleton-Set X) X))))
(define (singleton-set-ops a)
  (define empty (empty-singleton-set a))
  (define univ   (univ-singleton-set a))
  (set empty
       univ
       ;; meet
       (λ: ([A : (Singleton-Set X)] [B : (Singleton-Set X)])
         (cond [(or (empty-singleton-set? A) (empty-singleton-set? B))  empty]
               [else  univ]))
       ;; join
       (λ: ([A : (Singleton-Set X)] [B : (Singleton-Set X)])
         (cond [(or (univ-singleton-set? A) (univ-singleton-set? B))  univ]
               [else  empty]))
       ;; member?
       (λ: ([A : (Singleton-Set X)] [a : X])
         (if (empty-singleton-set? A) #f #t))
       ;; singleton
       (λ: ([a : X]) univ)))

(define-type Null-Set (Singleton-Set Null))
(define null-set-ops ((inst singleton-set-ops Null) null))

;; ---------------------------------------------------------------------------------------------------
;; Sets containing ⊥ (e.g. sets of (Maybe Bool))

(struct: (S) only-just ([set : S]) #:transparent)
(struct: (S) with-bot ([set : S]) #:transparent)
(define-type (Maybe-Set S) (U (only-just S) (with-bot S)))

(: without-bot (All (S) ((Maybe-Set S) -> S)))
(define (without-bot A)
  (match A
    [(only-just A)  A]
    [(with-bot A)   A]))

;; instance set S X => set (Maybe-Set S) (Maybe X) where ...
(: maybe-set-ops (All (S X) ((set S X) -> (set (Maybe-Set S) (Maybe X)))))
(define (maybe-set-ops ops)
  (set (only-just (set-empty ops))
       (with-bot  (set-univ  ops))
       ;; meet
       (λ: ([A : (Maybe-Set S)] [B : (Maybe-Set S)])
         (match* (A B)
           [((with-bot A)  (with-bot B))   (with-bot  ((set-meet ops) A B))]
           [((with-bot A)  (only-just B))  (only-just ((set-meet ops) A B))]
           [((only-just A) (with-bot B))   (only-just ((set-meet ops) A B))]
           [((only-just A) (only-just B))  (only-just ((set-meet ops) A B))]))
       ;; join
       (λ: ([A : (Maybe-Set S)] [B : (Maybe-Set S)])
         (match* (A B)
           [((with-bot A)  (with-bot B))   (with-bot  ((set-join ops) A B))]
           [((with-bot A)  (only-just B))  (with-bot  ((set-join ops) A B))]
           [((only-just A) (with-bot B))   (with-bot  ((set-join ops) A B))]
           [((only-just A) (only-just B))  (only-just ((set-join ops) A B))]))
       ;; member?
       (λ: ([A : (Maybe-Set S)] [b : (Maybe X)])
         (match* (A b)
           [((with-bot A)  (just b))  ((set-member? ops) A b)]
           [((only-just A) (just b))  ((set-member? ops) A b)]
           [((with-bot A)  _)  #t]
           [((only-just A) _)  #f]))
       ;; singleton
       (λ: ([a : (Maybe X)])
         (match a
           [(just a)  (only-just ((set-singleton ops) a))]
           [_         (with-bot  (set-empty ops))]))))
