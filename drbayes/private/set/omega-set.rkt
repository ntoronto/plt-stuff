#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/list
         racket/promise
         math/flonum
         "types.rkt"
         "extremal-set.rkt"
         "real-set.rkt"
         "bool-set.rkt"
         "pair-set.rkt"
         "omega-value.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Tree set types

(struct: Base-Tree-Set Base-Bot-Basic () #:transparent)
(define tree-set? Base-Tree-Set?)

(struct: (S) Tree-Rect Base-Tree-Set ([values : S]
                                      [children : (Nonempty-Tree-Children-Set S)])
  #:transparent)

(define-singleton-type Empty-Tree-Set Base-Tree-Set empty-tree-set)
(define-singleton-type Full-Tree-Set Base-Tree-Set full-tree-set)
(define-type (Nonempty-Tree-Set S) (U (Tree-Rect S) Full-Tree-Set))
(define-type (Nonfull-Tree-Set S) (U (Tree-Rect S) Empty-Tree-Set))
(define-type (Tree-Set S) (U (Tree-Rect S) Full-Tree-Set Empty-Tree-Set))

(struct: (S) Tree-Children-Rect ([fst : (Nonempty-Tree-Set S)]
                                 [snd : (Nonempty-Tree-Set S)])
  #:transparent)

(define-singleton-type Empty-Tree-Children-Set empty-tree-children-set)
(define-singleton-type Full-Tree-Children-Set full-tree-children-set)
(define-type (Nonempty-Tree-Children-Set S) (U (Tree-Children-Rect S) Full-Tree-Children-Set))
(define-type (Nonfull-Tree-Children-Set S) (U (Tree-Children-Rect S) Empty-Tree-Children-Set))
(define-type (Tree-Children-Set S)
  (U (Tree-Children-Rect S) Full-Tree-Children-Set Empty-Tree-Children-Set))

;; ===================================================================================================

(define-type Omega-Rect (Tree-Rect Nonextremal-Real-Set))
(define-type Nonfull-Omega-Set (Nonfull-Tree-Set Nonextremal-Real-Set))
(define-type Nonempty-Omega-Set (Nonempty-Tree-Set Nonextremal-Real-Set))
(define-type Omega-Set (Tree-Set Nonextremal-Real-Set))

(define-type Omega-Children-Rect (Tree-Children-Rect Nonextremal-Real-Set))
(define-type Nonfull-Omega-Children-Set (Nonfull-Tree-Children-Set Nonextremal-Real-Set))
(define-type Nonempty-Omega-Children-Set (Nonempty-Tree-Children-Set Nonextremal-Real-Set))
(define-type Omega-Children-Set (Tree-Children-Set Nonextremal-Real-Set))

(: omega-rect (Nonempty-Real-Set Nonempty-Omega-Children-Set -> Omega-Rect))
(define (omega-rect A1 A2)
  (define B1 (real-set-intersect A1 unit-interval))
  (cond [(empty-real-set? B1)
         (raise-argument-error 'omega-rect "nonempty subset of unit-interval" A1)]
        [else
         (Tree-Rect B1 A2)]))

;; ===================================================================================================

(define-type Branches-Rect (Tree-Rect Nonempty-Bool-Set))
(define-type Nonfull-Branches-Set (Nonfull-Tree-Set Nonempty-Bool-Set))
(define-type Nonempty-Branches-Set (Nonempty-Tree-Set Nonempty-Bool-Set))
(define-type Branches-Set (Tree-Set Nonempty-Bool-Set))

(define-type Branches-Children-Rect (Tree-Children-Rect Nonempty-Bool-Set))
(define-type Nonfull-Branches-Children-Set (Nonfull-Tree-Children-Set Nonempty-Bool-Set))
(define-type Nonempty-Branches-Children-Set (Nonempty-Tree-Children-Set Nonempty-Bool-Set))
(define-type Branches-Children-Set (Tree-Children-Set Nonempty-Bool-Set))

(: branches-rect (Nonempty-Bool-Set Nonempty-Branches-Children-Set -> Branches-Rect))
(define branches-rect Tree-Rect)

;; ===================================================================================================
;; Omega signatures and operations

(define-syntax real-set-sig
  (set-sig
   #'(Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   #'real-set-member?
   #'reals?
   #'empty-real-set?
   #'reals
   #'empty-real-set
   #'real-set-intersect
   #'real-set-union
   #'real-set-subseteq?))

(define-syntax omega-set-sig
  (set-sig
   #'(Omega-Rect Full-Tree-Set Empty-Tree-Set Omega)
   #'omega-set-member?
   #'full-tree-set?
   #'empty-tree-set?
   #'full-tree-set
   #'empty-tree-set
   #'omega-set-intersect
   #'omega-set-join
   #'omega-set-subseteq?))

(define-syntax omega-children-set-sig
  (set-sig
   #'(Omega-Children-Rect Full-Tree-Children-Set Empty-Tree-Children-Set Omega-Children)
   #'omega-children-set-member?
   #'full-tree-children-set?
   #'empty-tree-children-set?
   #'full-tree-children-set
   #'empty-tree-children-set
   #'omega-children-set-intersect
   #'omega-children-set-join
   #'omega-children-set-subseteq?))

(define-syntax omega-rect-sig
  (rect-sig
   (syntax-local-value #'omega-set-sig)
   (syntax-local-value #'real-set-sig)
   (syntax-local-value #'omega-children-set-sig)
   #'omega-rect #'Tree-Rect-values #'Tree-Rect-children
   #'Omega #'omega-value #'omega-children))

(define-syntax omega-children-rect-sig
  (rect-sig
   (syntax-local-value #'omega-children-set-sig)
   (syntax-local-value #'omega-set-sig)
   (syntax-local-value #'omega-set-sig)
   #'Tree-Children-Rect #'Tree-Children-Rect-fst #'Tree-Children-Rect-snd
   #'Omega-Children #'Tree-Children-fst #'Tree-Children-snd))

(define-rect-constructor omega-set omega-rect-sig)
(define-rect-ops omega-set omega-rect-sig)

(define-rect-constructor omega-children-set omega-children-rect-sig)
(define-rect-ops omega-children-set omega-children-rect-sig)

;; ===================================================================================================
;; Branches signatures and operations

(define-syntax bool-set-sig
  (set-sig
   #'(Nonextremal-Bool-Set Full-Bool-Set Empty-Bool-Set Boolean)
   #'bool-set-member?
   #'bools?
   #'empty-bool-set?
   #'bools
   #'empty-bool-set
   #'bool-set-intersect
   #'bool-set-union
   #'bool-set-subseteq?))

(define-syntax branches-set-sig
  (set-sig
   #'(Branches-Rect Full-Tree-Set Empty-Tree-Set Branches)
   #'branches-set-member?
   #'full-tree-set?
   #'empty-tree-set?
   #'full-tree-set
   #'empty-tree-set
   #'branches-set-intersect
   #'branches-set-join
   #'branches-set-subseteq?))

(define-syntax branches-children-set-sig
  (set-sig
   #'(Branches-Children-Rect Full-Tree-Children-Set Empty-Tree-Children-Set Branches-Children)
   #'branches-children-set-member?
   #'full-tree-children-set?
   #'empty-tree-children-set?
   #'full-tree-children-set
   #'empty-tree-children-set
   #'branches-children-set-intersect
   #'branches-children-set-join
   #'branches-children-set-subseteq?))

(define-syntax branches-rect-sig
  (rect-sig
   (syntax-local-value #'branches-set-sig)
   (syntax-local-value #'bool-set-sig)
   (syntax-local-value #'branches-children-set-sig)
   #'branches-rect #'Tree-Rect-values #'Tree-Rect-children
   #'Branches #'branches-value #'branches-children))

(define-syntax branches-children-rect-sig
  (rect-sig
   (syntax-local-value #'branches-children-set-sig)
   (syntax-local-value #'branches-set-sig)
   (syntax-local-value #'branches-set-sig)
   #'Tree-Children-Rect #'Tree-Children-Rect-fst #'Tree-Children-Rect-snd
   #'Branches-Children #'Tree-Children-fst #'Tree-Children-snd))

(define-rect-constructor branches-set branches-rect-sig)
(define-rect-ops branches-set branches-rect-sig)

(define-rect-constructor branches-children-set branches-children-rect-sig)
(define-rect-ops branches-children-set branches-children-rect-sig)

;; ===================================================================================================
;; Ref

(: make-tree-set-ref (All (S E) (S E -> (case-> ((Nonempty-Tree-Set S) Omega-Index -> S)
                                                ((Tree-Set S) Omega-Index -> (U S E))))))
(define (make-tree-set-ref full empty)
  
  (: tree-set-ref (case-> ((Nonempty-Tree-Set S) Omega-Index -> S)
                          ((Tree-Set S) Omega-Index -> (U S E))))
  (define (tree-set-ref A r)
    (cond [(empty-tree-set? A)  empty]
          [else  (tree-set-ref* A r)]))
  
  (: tree-set-ref* ((Nonempty-Tree-Set S) Omega-Index -> S))
  (define (tree-set-ref* A r)
    (cond [(full-tree-set? A)  full]
          [else  (match-define (Tree-Rect A1 A2) A)
                 (if (empty? r) A1 (tree-children-set-ref A2 r))]))
  
  (: tree-children-set-ref ((Nonempty-Tree-Children-Set S) Nonempty-Omega-Index -> S))
  (define (tree-children-set-ref A r)
    (cond [(full-tree-children-set? A)  full]
          [else  (match-define (Tree-Children-Rect A1 A2) A)
                 (tree-set-ref* (if (zero? (first r)) A1 A2) (rest r))]))
  
  tree-set-ref)

(define omega-set-ref
  ((inst make-tree-set-ref Nonextremal-Real-Set Empty-Real-Set) unit-interval empty-real-set))

(define branches-set-ref
  ((inst make-tree-set-ref Nonempty-Bool-Set Empty-Bool-Set) bools empty-bool-set))

;; ===================================================================================================
;; Restriction (functional update with intersection)

(: make-tree-set-restrict
   (All (S E) (((U S E) (Tree-Children-Set S) -> (Tree-Set S))
               ((Tree-Set S) (Tree-Set S) -> (Tree-Children-Set S))
               S
               (Any -> Boolean : E)
               (S S -> (U S E))
               -> ((Tree-Set S) Omega-Index (U S E) -> (Tree-Set S)))))
(define (make-tree-set-restrict make-set make-children-set full-set empty-set? intersect)
  
  (: tree-set-restrict ((Tree-Set S) Omega-Index (U S E) -> (Tree-Set S)))
  (define (tree-set-restrict A r B)
    (cond [(or (empty-tree-set? A) (empty-set? B))  empty-tree-set]
          [else  (tree-set-restrict* A r B)]))
  
  (: tree-set-restrict* ((Nonempty-Tree-Set S) Omega-Index S -> (Tree-Set S)))
  (define (tree-set-restrict* A r B)
    (cond [(full-tree-set? A)
           (tree-set-restrict* (Tree-Rect full-set full-tree-children-set) r B)]
          [else  (match-define (Tree-Rect A1 A2) A)
                 (if (empty? r)
                     (make-set (intersect A1 B) A2)
                     (make-set A1 (tree-children-set-restrict A2 r B)))]))
  
  (: tree-children-set-restrict ((Nonempty-Tree-Children-Set S) Nonempty-Omega-Index S
                                                                -> (Tree-Children-Set S)))
  (define (tree-children-set-restrict A r B)
    (cond [(full-tree-children-set? A)
           (tree-children-set-restrict (Tree-Children-Rect full-tree-set full-tree-set) r B)]
          [else  (match-define (Tree-Children-Rect A1 A2) A)
                 (if (zero? (first r))
                     (make-children-set (tree-set-restrict* A1 (rest r) B) A2)
                     (make-children-set A1 (tree-set-restrict* A2 (rest r) B)))]))
  
  tree-set-restrict)

(define omega-set-restrict*
  ((inst make-tree-set-restrict Nonextremal-Real-Set Empty-Real-Set)
   omega-set
   omega-children-set
   unit-interval
   empty-real-set?
   real-set-intersect))

(: omega-set-restrict (Omega-Set Omega-Index Real-Set -> Omega-Set))
(define (omega-set-restrict A r B)
  (omega-set-restrict* A r (real-set-intersect B unit-interval)))

(define branches-set-restrict
  ((inst make-tree-set-restrict Nonempty-Bool-Set Empty-Bool-Set)
   branches-set
   branches-children-set
   bools
   empty-bool-set?
   bool-set-intersect))

;; ===================================================================================================
;; Set (functional update)

(: make-tree-set-set
   (All (S) ((S (Nonempty-Tree-Children-Set S) -> (Nonempty-Tree-Set S))
             ((Nonempty-Tree-Set S) (Nonempty-Tree-Set S) -> (Nonempty-Tree-Children-Set S))
             S
             -> ((Nonempty-Tree-Set S) Omega-Index S -> (Nonempty-Tree-Set S)))))
(define (make-tree-set-set make-set make-children-set full-set)
  
  (: tree-set-set ((Nonempty-Tree-Set S) Omega-Index S -> (Nonempty-Tree-Set S)))
  (define (tree-set-set A r B)
    (cond [(full-tree-set? A)
           (tree-set-set (Tree-Rect full-set full-tree-children-set) r B)]
          [else  (match-define (Tree-Rect A1 A2) A)
                 (if (empty? r)
                     (make-set B A2)
                     (make-set A1 (tree-children-set-set A2 r B)))]))
  
  (: tree-children-set-set ((Nonempty-Tree-Children-Set S) Nonempty-Omega-Index S
                                                           -> (Nonempty-Tree-Children-Set S)))
  (define (tree-children-set-set A r B)
    (cond [(full-tree-children-set? A)
           (tree-children-set-set (Tree-Children-Rect full-tree-set full-tree-set) r B)]
          [else  (match-define (Tree-Children-Rect A1 A2) A)
                 (if (zero? (first r))
                     (make-children-set (tree-set-set A1 (rest r) B) A2)
                     (make-children-set A1 (tree-set-set A2 (rest r) B)))]))
  
  tree-set-set)

(define omega-set-set*
  ((inst make-tree-set-set Nonextremal-Real-Set)
   omega-set
   omega-children-set
   unit-interval))

(: omega-set-set (Nonempty-Omega-Set Omega-Index Nonextremal-Real-Set -> Nonempty-Omega-Set))
(define (omega-set-set A r orig-B)
  (define B (real-set-intersect orig-B unit-interval))
  (cond [(empty-real-set? B)
         (raise-argument-error 'omega-set-set "nonempty subset of unit-interval" orig-B)]
        [else
         (omega-set-set* A r B)]))

(define branches-set-set
  ((inst make-tree-set-set Nonempty-Bool-Set)
   branches-set
   branches-children-set
   bools))

;; ===================================================================================================
;; Sampling

(: make-tree-set-sample-point
   (All (S V) ((S -> (U V Random-Value)) -> ((Nonempty-Tree-Set S) -> (Tree V)))))
(define (make-tree-set-sample-point sample-point)
  (: tree-set-sample-point ((Nonempty-Tree-Set S) -> (Tree V)))
  (define (tree-set-sample-point A)
    (cond [(full-tree-set? A)  (random-tree)]
          [else  (match-define (Tree-Rect A1 A2) A)
                 ((inst box (U (Tree-Node V) Random-Value))
                  (Tree-Node ((inst box (U V Random-Value)) (sample-point A1))
                             (tree-children-set-sample-point A2)))]))
  
  (: tree-children-set-sample-point ((Nonempty-Tree-Children-Set S) -> (Tree-Children V)))
  (define (tree-children-set-sample-point A)
    (cond [(full-tree-children-set? A)  (random-tree-children)]
          [else  (match-define (Tree-Children-Rect A1 A2) A)
                 (Tree-Children (tree-set-sample-point A1)
                                (tree-set-sample-point A2))]))
  
  tree-set-sample-point)

(define omega-set-sample-point
  ((inst make-tree-set-sample-point Nonextremal-Real-Set Flonum)
   (λ (I) (cond [(reals? I)  (raise-argument-error 'omega-set-sample-point "Nonextremal-Real-Set" I)]
                [(equal? I unit-interval)  random-value]
                [else  (real-set-sample-point I)]))))

(define branches-set-sample-point
  ((inst make-tree-set-sample-point Nonempty-Bool-Set Boolean)
   (λ (B) (cond [(bools? B)  random-value]
                [(trues? B)  #t]
                [else  #f]))))

;; ===================================================================================================
;; Measurement

(: omega-set-measure (Omega-Set -> Flonum))
(define (omega-set-measure A)
  (cond [(full-tree-set? A)  1.0]
        [(empty-tree-set? A)  0.0]
        [else  (match-define (Tree-Rect A1 A2) A)
               (fl* (real-set-measure A1) (omega-children-set-measure A2))]))

(: omega-children-set-measure (Omega-Children-Set -> Flonum))
(define (omega-children-set-measure A)
  (cond [(full-tree-children-set? A)  1.0]
        [(empty-tree-children-set? A)  0.0]
        [else  (match-define (Tree-Children-Rect A1 A2) A)
               (fl* (omega-set-measure A1) (omega-set-measure A2))]))

;; ===================================================================================================
;; Conversion

(: make-tree-set->list (All (S) ((S -> Boolean) -> ((Nonempty-Tree-Set S) -> (Listof S)))))
(define ((make-tree-set->list full?) A)
  (: As (Listof S))
  (define As
    (let: loop ([A A] [As : (Listof S)  null])
      (cond [(full-tree-set? A)  As]
            [else
             (match-define (Tree-Rect A1 A2) A)
             (cond [(full-tree-children-set? A2)
                    (loop (Tree-Rect A1 (Tree-Children-Rect full-tree-set full-tree-set)) As)]
                   [else
                    (match-define (Tree-Children-Rect B1 B2) A2)
                    (let ([As  (loop B1 As)])
                      (cond [(full? A1)  (loop B2 As)]
                            [else  (loop B2 (cons A1 As))]))])])))
  (reverse As))

(define omega-set->list
  ((inst make-tree-set->list Nonextremal-Real-Set) (λ (I) (equal? I unit-interval))))

(define branches-set->list
  ((inst make-tree-set->list Nonempty-Bool-Set) bools?))
