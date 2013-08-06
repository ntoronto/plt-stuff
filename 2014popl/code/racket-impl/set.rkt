#lang typed/racket/base

(require "../types.rkt"
         "../untyped-utils.rkt"
         racket/match
         racket/list)

(provide (all-defined-out))

(define-singleton-type Empty-Set empty-set)
(define-singleton-type Univ-Set univ-set)

(define-type Set (U Empty-Set Univ-Set Real-Set Bool-Set Pair-Set Null-Set Tree-Set))

(define-type Value (Rec V (U Flonum Boolean (Pair V V) Tree-Val)))


(: set-empty? (Set -> Boolean))
(define (set-empty? A)
  (or (empty-set? A)
      (empty-real-set? A)
      (empty-bool-set? A)
      (empty-pair-set? A)
      (empty-null-set? A)))

(: set-meet (Set Set -> Set))
(define (set-meet A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-meet A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-meet A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-meet A B)]
        [(and (null-set? A) (null-set? B))  (null-set-meet A B)]
        [(and (tree-set? A) (tree-set? B))  (tree-set-meet A B)]
        [(or (empty-set? A) (empty-set? B))  empty-set]
        [(univ-set? A)  B]
        [(univ-set? B)  A]
        [else  empty-set]))

(: set-join (Set Set -> Set))
(define (set-join A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-join A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-join A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-join A B)]
        [(and (null-set? A) (null-set? B))  (null-set-join A B)]
        [(and (tree-set? A) (tree-set? B))  (tree-set-join A B)]
        [(or (univ-set? A) (univ-set? B))  univ-set]
        [(empty-set? A)  B]
        [(empty-set? B)  A]
        [else  univ-set]))

(: set-member? (Set Value -> Boolean))
(define (set-member? A a)
  (cond [(and (real-set? A) (flonum? a))  (real-set-member? A a)]
        [(and (bool-set? A) (boolean? a))  (bool-set-member? A a)]
        [(and (pair-set? A) (pair? a))  (pair-set-member? A a)]
        [(and (null-set? A) (null? a))  (null-set-member? A a)]
        [(and (tree-set? A) (tree-val? a))  (tree-set-member? A a)]
        [(univ-set? A)  #t]
        [else  #f]))

(: set-singleton (Value -> Set))
(define (set-singleton a)
  (cond [(flonum? a)  (real-set-singleton a)]
        [(boolean? a)  (bool-set-singleton a)]
        [(pair? a)  (pair-set-singleton a)]
        [(null? a)  (null-set-singleton a)]
        [(tree-val? a)  (tree-set-singleton a)]))

(: set-proj-fst (Set -> Set))
(define (set-proj-fst A)
  (cond [(pair-set? A)  (pair-set-proj-fst A)]
        [(univ-set? A)  univ-set]
        [else  empty-set]))

(: set-proj-snd (Set -> Set))
(define (set-proj-snd A)
  (cond [(pair-set? A)  (pair-set-proj-snd A)]
        [(univ-set? A)  univ-set]
        [else  empty-set]))

(: set-project (Tree-Index Set -> Set))
(define (set-project j A)
  (cond [(tree-set? A)  (tree-set-project j A)]
        [(univ-set? A)  (tree-set-project j univ-tree-set)]
        [else  empty-set]))

(: set-unproject (Tree-Index Set Set -> Set))
(define (set-unproject j A B)
  (cond [(tree-set? A)  (tree-set-unproject j A B)]
        [(univ-set? A)  (tree-set-unproject j univ-tree-set B)]
        [else  empty-set]))

;; ---------------------------------------------------------------------------------------------------
;; Reals

(struct: Real-Set-Base () #:transparent)
(struct: interval Real-Set-Base ([min : Flonum] [max : Flonum]) #:transparent)
(define-singleton-type Empty-Real-Set Real-Set-Base empty-real-set)
(define-singleton-type Univ-Real-Set Real-Set-Base univ-real-set)

(define-type Real-Set (U interval Empty-Real-Set Univ-Real-Set))
(define real-set? Real-Set-Base?)

(define real-ivl (interval -inf.0 +inf.0))
(define unit-ivl (interval 0.0 1.0))

(: ivl (Flonum Flonum -> Real-Set))
(define (ivl min max)
  (cond [(and (min . = . -inf.0) (max . = . +inf.0))  univ-real-set]
        [(min . <= . max)  (interval min max)]
        [else  empty-real-set]))

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

;; ---------------------------------------------------------------------------------------------------
;; Booleans

(struct: Bool-Set-Base () #:transparent)
(struct: singleton-bool-set Bool-Set-Base ([value : Boolean])
  #:transparent
  #:property prop:custom-write
  (λ (A port mode)
    (fprintf port (if (singleton-bool-set-value A) "true-set" "false-set")))
  )

(define-singleton-type Empty-Bool-Set Bool-Set-Base empty-bool-set)
(define-singleton-type Univ-Bool-Set Bool-Set-Base univ-bool-set)

(define-type Bool-Set (U singleton-bool-set Empty-Bool-Set Univ-Bool-Set))
(define bool-set? Bool-Set-Base?)

(define true-set (singleton-bool-set #t))
(define false-set (singleton-bool-set #f))

(: bool-set-meet (Bool-Set Bool-Set -> Bool-Set))
(define (bool-set-meet A B)
  (cond [(or (empty-bool-set? A) (empty-bool-set? B))  empty-bool-set]
        [(univ-bool-set? A)  B]
        [(univ-bool-set? B)  A]
        [(eq? (singleton-bool-set-value A) (singleton-bool-set-value B))  A]
        [else  empty-bool-set]))

(: bool-set-join (Bool-Set Bool-Set -> Bool-Set))
(define (bool-set-join A B)
  (cond [(or (univ-bool-set? A) (univ-bool-set? B))  univ-bool-set]
        [(empty-bool-set? A)  B]
        [(empty-bool-set? B)  A]
        [(eq? (singleton-bool-set-value A) (singleton-bool-set-value B))  A]
        [else  univ-bool-set]))

(: bool-set-member? (Bool-Set Boolean -> Boolean))
(define (bool-set-member? A a)
  (cond [(empty-bool-set? A)  #f]
        [(univ-bool-set? A)   #t]
        [else  (eq? (singleton-bool-set-value A) a)]))

(: bool-set-singleton (Boolean -> Bool-Set))
(define (bool-set-singleton a)
  (singleton-bool-set a))

;; ---------------------------------------------------------------------------------------------------
;; Pairs

(struct: Pair-Set-Base () #:transparent)
(struct: prod-set Pair-Set-Base ([fst : Set] [snd : Set]) #:transparent)
(define-singleton-type Empty-Pair-Set Pair-Set-Base empty-pair-set)
(define-singleton-type Univ-Pair-Set Pair-Set-Base univ-pair-set)

(define-type Pair-Set (U prod-set Empty-Pair-Set Univ-Pair-Set))
(define pair-set? Pair-Set-Base?)

(: set-prod (Set Set -> Pair-Set))
(define (set-prod A1 A2)
  (cond [(or (set-empty? A1) (set-empty? A2))  empty-pair-set]
        [(and (univ-set? A1) (univ-set? A2))   univ-pair-set]
        [else  (prod-set A1 A2)]))

(: pair-set-proj-fst (Pair-Set -> Set))
(define (pair-set-proj-fst A)
  (cond [(empty-pair-set? A)  empty-set]
        [(univ-pair-set? A)  univ-set]
        [else  (prod-set-fst A)]))

(: pair-set-proj-snd (Pair-Set -> Set))
(define (pair-set-proj-snd A)
  (cond [(empty-pair-set? A)  empty-set]
        [(univ-pair-set? A)  univ-set]
        [else  (prod-set-snd A)]))

(: pair-set-meet (Pair-Set Pair-Set -> Pair-Set))
(define (pair-set-meet A B)
  (cond [(or (empty-pair-set? A) (empty-pair-set? B))  empty-pair-set]
        [(univ-pair-set? A)  B]
        [(univ-pair-set? B)  A]
        [else  (match-define (prod-set A1 A2) A)
               (match-define (prod-set B1 B2) B)
               (set-prod (set-meet A1 B1) (set-meet A2 B2))]))

(: pair-set-join (Pair-Set Pair-Set -> Pair-Set))
(define (pair-set-join A B)
  (cond [(or (univ-pair-set? A) (univ-pair-set? B))  univ-pair-set]
        [(empty-pair-set? A)  B]
        [(empty-pair-set? B)  A]
        [else  (match-define (prod-set A1 A2) A)
               (match-define (prod-set B1 B2) B)
               (prod-set (set-join A1 B1) (set-join A2 B2))]))

(: pair-set-member? (Pair-Set (Pair Value Value) -> Boolean))
(define (pair-set-member? A a)
  (cond [(empty-pair-set? A)  #f]
        [(univ-pair-set? A)   #t]
        [else  (match-define (prod-set A1 A2) A)
               (match-define (cons a1 a2) a)
               (and (set-member? A1 a1) (set-member? A2 a2))]))

(: pair-set-singleton ((Pair Value Value) -> Pair-Set))
(define (pair-set-singleton a)
  (match-define (cons a1 a2) a)
  (set-prod (set-singleton a1) (set-singleton a2)))

;; ---------------------------------------------------------------------------------------------------
;; Nulls

(struct: Base-Null-Set () #:transparent)
(define-singleton-type Empty-Null-Set Base-Null-Set empty-null-set)
(define-singleton-type Univ-Null-Set Base-Null-Set univ-null-set)

(define-type Null-Set (U Empty-Null-Set Univ-Null-Set))
(define null-set? Base-Null-Set?)

(: null-set-meet (Null-Set Null-Set -> Null-Set))
(define (null-set-meet A B)
  (cond [(or (empty-null-set? A) (empty-null-set? B))  empty-null-set]
        [(univ-null-set? A)  B]
        [(univ-null-set? B)  A]))

(: null-set-join (Null-Set Null-Set -> Null-Set))
(define (null-set-join A B)
  (cond [(or (univ-null-set? A) (univ-null-set? B))  univ-null-set]
        [(empty-null-set? A)  B]
        [(empty-null-set? B)  A]))

(: null-set-member? (Null-Set Null -> Boolean))
(define (null-set-member? A a)
  (cond [(empty-null-set? A)  #f]
        [(univ-null-set? A)   #t]))

(: null-set-singleton (Null -> Null-Set))
(define (null-set-singleton a)
  univ-null-set)

#|
;; ---------------------------------------------------------------------------------------------------
;; Sets containing ⊥ (e.g. sets of (Maybe Bool))

(struct: Maybe-Set-Base () #:transparent)
(struct: only-just Maybe-Set-Base ([set : Set]) #:transparent)
(struct: with-bot Maybe-Set-Base ([set : Set]) #:transparent)

(define-type Maybe-Set (U only-just with-bot))
(define maybe-set? Maybe-Set-Base?)

(: maybe-set-subtract-bot (Maybe-Set -> Set))
(define (maybe-set-subtract-bot A)
  (match A
    [(only-just A)  A]
    [(with-bot A)   A]))

(: maybe-set-meet (Maybe-Set Maybe-Set -> Maybe-Set))
(define (maybe-set-meet A B)
  (match* (A B)
    [((with-bot A)  (with-bot B))   (with-bot  (set-meet A B))]
    [((with-bot A)  (only-just B))  (only-just (set-meet A B))]
    [((only-just A) (with-bot B))   (only-just (set-meet A B))]
    [((only-just A) (only-just B))  (only-just (set-meet A B))]))

(: maybe-set-join (Maybe-Set Maybe-Set -> Maybe-Set))
(define (maybe-set-join A B)
  (match* (A B)
    [((with-bot A)  (with-bot B))   (with-bot  (set-join A B))]
    [((with-bot A)  (only-just B))  (with-bot  (set-join A B))]
    [((only-just A) (with-bot B))   (with-bot  (set-join A B))]
    [((only-just A) (only-just B))  (only-just (set-join A B))]))

(: maybe-set-member? (Maybe-Set (Maybe Value) -> Boolean))
(define (maybe-set-member? A a)
  (match* (A a)
    [((with-bot A)  (just a))  (set-member? A a)]
    [((only-just A) (just a))  (set-member? A a)]
    [((with-bot A)  _)  #t]
    [((only-just A) _)  #f]))

(: maybe-set-singleton ((Maybe Value) -> Maybe-Set))
(define (maybe-set-singleton a)
  (match a
    [(just a)  (only-just (set-singleton a))]
    [_         (with-bot  empty-set)]))
|#

;; ===================================================================================================
;; Sets of countable vectors with at most finitely many restricted axes

(define-type Tree-Index (Listof Boolean))

(: index-left (Tree-Index -> Tree-Index))
(define (index-left j) (cons #t j))

(: index-right (Tree-Index -> Tree-Index))
(define (index-right j) (cons #f j))

(define j0 '())

;; ---------------------------------------------------------------------------------------------------

(struct: Tree-Val-Base () #:transparent)
(struct: tree-val-node Tree-Val-Base ([value : Value] [left : Tree-Val] [right : Tree-Val])
  #:transparent)
(define-singleton-type Any-Tree-Val Tree-Val-Base any-tree-val)

(define-type Tree-Val (U Any-Tree-Val tree-val-node))
(define tree-val? Tree-Val-Base?)

(: tree-val-ref (All (X) (Tree-Index Tree-Val -> Value)))
(define (tree-val-ref j t)
  (define j0 j)
  (let loop ([j j] [t t])
    (match t
      [(? any-tree-val?)  (error 'tree-val-ref "indeterminate value at ~e" j0)]
      [(tree-val-node x l r)  (cond [(empty? j)  x]
                                    [(first j)  (loop (rest j) l)]
                                    [else       (loop (rest j) r)])])))

;; ---------------------------------------------------------------------------------------------------

(struct: Base-Tree-Set () #:transparent)
(struct: Tree-Set-Node Base-Tree-Set ([axis : Set] [left : Tree-Set] [right : Tree-Set])
  #:transparent)
(define-singleton-type Empty-Tree-Set Base-Tree-Set empty-tree-set)
(define-singleton-type Univ-Tree-Set Base-Tree-Set univ-tree-set)

(define-type Tree-Set (U Empty-Tree-Set Univ-Tree-Set Tree-Set-Node))
(define tree-set? Base-Tree-Set?)

(: tree-set-node (Set Tree-Set Tree-Set -> Tree-Set))
(define (tree-set-node A L R)
  (cond [(or (set-empty? A) (empty-tree-set? L) (empty-tree-set? R))  empty-tree-set]
        [(and (univ-set? A) (univ-tree-set? L) (univ-tree-set? R))  univ-tree-set]
        [else  (Tree-Set-Node A L R)]))

(: tree-set-meet (Tree-Set Tree-Set -> Tree-Set))
(define (tree-set-meet A1 A2)
  (cond [(or (empty-tree-set? A1) (empty-tree-set? A2))  empty-tree-set]
        [(univ-tree-set? A1)  A2]
        [(univ-tree-set? A2)  A1]
        [else  (match-let ([(Tree-Set-Node A1 L1 R1)  A1]
                           [(Tree-Set-Node A2 L2 R2)  A2])
                 (tree-set-node (set-meet A1 A2) (tree-set-meet L1 L2) (tree-set-meet R1 R2)))]))

(: tree-set-join (Tree-Set Tree-Set -> Tree-Set))
(define (tree-set-join A1 A2)
  (cond [(or (univ-tree-set? A1) (univ-tree-set? A2))  univ-tree-set]
        [(empty-tree-set? A1)  A2]
        [(empty-tree-set? A2)  A1]
        [else  (match-let ([(Tree-Set-Node A1 L1 R1)  A1]
                           [(Tree-Set-Node A2 L2 R2)  A2])
                 (tree-set-node (set-join A1 A2) (tree-set-join L1 L2) (tree-set-join R1 R2)))]))

(: tree-set-member? (Tree-Set Tree-Val -> Boolean))
(define (tree-set-member? A a)
  (cond [(empty-tree-set? A)  #f]
        [(univ-tree-set? A)   #t]
        [(any-tree-val? a)  (error 'tree-set-member? "indeterminate value")]
        [else  (match-let ([(Tree-Set-Node A L R)  A]
                           [(tree-val-node a l r)  a])
                 (and (set-member? A a) (tree-set-member? L l) (tree-set-member? R r)))]))

(: tree-set-singleton (Tree-Val -> Tree-Set))
(define (tree-set-singleton a)
  (match a
    [(? any-tree-val?)  univ-tree-set]
    [(tree-val-node a l r)  (tree-set-node (set-singleton a)
                                           (tree-set-singleton l)
                                           (tree-set-singleton r))]))

(: tree-set-project (Tree-Index Tree-Set -> Set))
(define (tree-set-project j A)
  (cond [(empty-tree-set? A)  empty-set]
        [(univ-tree-set? A)   univ-set]
        [(empty? j)  (Tree-Set-Node-axis A)]
        [(first j)   (tree-set-project (rest j) (Tree-Set-Node-left A))]
        [else        (tree-set-project (rest j) (Tree-Set-Node-right A))]))

(: tree-set-unproject (Tree-Index Tree-Set Set -> Tree-Set))
(define (tree-set-unproject j A B)
  (let loop ([j j] [A A])
    (cond [(empty-tree-set? A)  empty-tree-set]
          [(univ-tree-set? A)   (loop j (Tree-Set-Node univ-set univ-tree-set univ-tree-set))]
          [else  (match-let ([(Tree-Set-Node A L R)  A])
                   (cond [(empty? j)  (tree-set-node (set-meet A B) L R)]
                         [(first j)   (tree-set-node A (loop (rest j) L) R)]
                         [else        (tree-set-node A L (loop (rest j) R))]))])))
