#lang typed/racket

;; Verify that rectangular sets and operations on them comprise bounded lattices w.r.t. the subset
;; partial order, using randomized testing

(require typed/rackunit
         math/distributions
         "../private/omega.rkt"
         "../private/rect.rkt")

(define-syntax-rule (implies a b) (or (not a) b))

(: flip (All (A B C) ((A B -> C) -> (B A -> C))))
(define ((flip f) x y) (f y x))

;; Using this is about 1000x faster than using `check-true' directly, mostly because it doesn't have
;; to construct the message unless there's a failure
(define-syntax-rule (check-prop expr msg)
  (if expr (void) (check-true expr msg)))

;; ===================================================================================================
;; Property checks

;; Partial order properties

(: check-reflexive (All (T) ((T T -> Boolean) T -> Any)))
(define (check-reflexive lte? A)
  (check-prop (lte? A A)
              (format "~a: reflexivity failed on ~v" lte? A)))

(: check-antisymmetric (All (T) ((T T -> Boolean) T T -> Any)))
(define (check-antisymmetric lte? A B)
  (check-prop (implies (and (lte? A B) (lte? B A))
                       (equal? A B))
              (format "~a: antisymmetry failed on ~v ~v" lte? A B)))

(: check-transitive (All (T) ((T T -> Boolean) T T T -> Any)))
(define (check-transitive lte? A B C)
  (check-prop (implies (and (lte? A B) (lte? B C))
                       (lte? A C))
              (format "~a: transitivity failed on ~v ~v ~v" lte? A B C)))

;; Lattice operator properties

(: check-identity (All (T) ((T T -> T) T T -> Any)))
(define (check-identity op id A)
  (check-prop (equal? (op id A) A)
              (format "~a ~a: identity failed on ~v" op id A)))

(: check-commutative (All (T) ((T T -> T) T T -> Any)))
(define (check-commutative op A B)
  (check-prop (equal? (op A B) (op B A))
              (format "~a: commutativity failed on ~v ~v" op A B)))

(: check-absorption (All (T) ((T T -> T) (T T -> T) T T -> Any)))
(define (check-absorption op1 op2 A B)
  (check-prop (equal? (op1 A (op2 A B)) A)
              (format "~a ~a: absorption failed on ~v ~v" op1 op2 A B)))

(: check-associative (All (T) ((T T -> T) T T T -> Any)))
(define (check-associative op A B C)
  (check-prop (equal? (op (op A B) C) (op A (op B C)))
              (format "~a: associativity failed on ~v ~v ~v" op A B C)))

;; Other properties (those that involve both the partial order and the operations)

(: check-nondecreasing (All (T) ((T T -> Boolean) (T T -> T) T T -> Any)))
(define (check-nondecreasing lte? op A B)
  (define C (op A B))
  (check-prop (and (lte? A C) (lte? B C))
              (format "~a ~a: nondecreasing failed on ~v ~v" lte? op A B)))

(: check-order-equiv (All (T) ((T T -> Boolean) (T T -> T) (T T -> T) T T -> Any)))
(define (check-order-equiv lte? join meet A B)
  (check-prop (eq? (or (equal? A (meet A B))
                       (equal? B (join A B)))
                   (lte? A B))
              (format "~a ~a ~a: equivalent order definition failed on ~v ~v" lte? join meet A B)))

(: check-monotone (All (T) ((T T -> Boolean) (T T -> T) T T T T -> Any)))
(define (check-monotone lte? op A1 A2 B1 B2)
  (check-prop (implies (and (lte? A1 A2) (lte? B1 B2))
                       (lte? (op A1 B1) (op A2 B2)))
              (format "~a ~a: monotonicity failed on ~v ~v ~v ~v" lte? op A1 A2 B1 B2)))

;; All properties

(: check-bounded-lattice (All (T) ((T T -> Boolean) (T T -> T) (T T -> T) T T (-> T) -> Any)))
(define (check-bounded-lattice lte? join meet bot top random)
  (define A (random))
  (define B (random))
  (define C (random))
  (define D (random))
  
  ;; Partial order properties
  
  (check-reflexive lte? A)
  (check-antisymmetric lte? A B)
  (check-transitive lte? A B C)
  
  ;; Lattice operator properties
  
  (check-identity join bot A)
  (check-identity meet top A)
  (check-commutative join A B)
  (check-commutative meet A B)
  (check-associative join A B C)
  (check-associative meet A B C)
  (check-absorption join meet A B)
  (check-absorption meet join A B)
  
  ;; Other properties
  
  (check-nondecreasing lte? join A B)
  (check-nondecreasing (flip lte?) meet A B)
  (check-order-equiv lte? join meet A B)
  (check-monotone lte? join A B C D)
  (check-monotone lte? meet A B C D))

;; ===================================================================================================
;; Rect tests

;; Using a discrete distribution for interval endpoints makes it more likely that two endpoints from
;; different intervals will be the same but one open and the other closed
(define real-endpoint-dist (discrete-dist '(-inf.0 -2.0 -1.0 -0.0 0.0 1.0 2.0 +inf.0)))

(: random-interval ((Discrete-Dist Flonum) -> Interval))
(define (random-interval dist)
  (define a (sample dist))
  (define b (sample dist))
  (define a? ((random) . < . 0.5))
  (define b? ((random) . < . 0.5))
  (define I (interval (min a b) (max a b) a? b?))
  (if (empty-set? I) (random-interval dist) I))

(: random-pair-rect (-> Pair-Rect))
(define (random-pair-rect)
  (define A (pair-rect (random-rect) (random-rect)))
  (if (empty-set? A) (random-pair-rect) A))

(: random-boolean-set (-> Boolean-Set))
(define (random-boolean-set)
  (define r (random))
  (cond [(r . < . #i1/3)  't]
        [(r . < . #i2/3)  'f]
        [else  'tf]))

(: random-join-rect (-> Join-Rect))
(define (random-join-rect)
  (define A (join-rect (and ((random) . < . 0.5) (random-interval real-endpoint-dist))
                       (and ((random) . < . 0.5) null-rect)
                       (and ((random) . < . 0.5) (random-pair-rect))
                       (and ((random) . < . 0.5) (random-boolean-set))))
  (if (join-rect? A) A (random-join-rect)))

(: random-rect (-> Rect))
(define (random-rect)
  (define r (random))
  (cond [(r . < . 0.10)  universal-set]
        [(r . < . 0.20)  empty-set]
        [(r . < . 0.30)  null-rect]
        [(r . < . 0.40)  (random-boolean-set)]
        [(r . < . 0.60)  (random-interval real-endpoint-dist)]
        [(r . < . 0.80)  (random-pair-rect)]
        [else  (random-join-rect)]))

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice rect-subseteq?
                          rect-join
                          rect-intersect
                          empty-set
                          universal-set
                          random-rect)))

;; ===================================================================================================
;; Omega-Rect tests

(: omega-rect-node-subseteq? ((Omega-Node Interval) (Omega-Node Interval) -> Boolean))
(define (omega-rect-node-subseteq? A B)
  (match-define (Omega-Node I1 A1 A2) A)
  (match-define (Omega-Node I2 B1 B2) B)
  (and (interval-subseteq? I1 I2)
       (omega-rect-subseteq? A1 B1)
       (omega-rect-subseteq? A2 B2)))

(: omega-rect-subseteq? ((U Empty-Set Omega-Rect) (U Empty-Set Omega-Rect) -> Boolean))
(define (omega-rect-subseteq? A B)
  (cond [(empty-set? A)  #t]
        [(empty-set? B)  #f]
        [(omega-leaf? B)  #t]
        [(omega-leaf? A)  #f]
        [(eq? A B)  #t]
        [else  (omega-rect-node-subseteq? A B)]))

(define unit-endpoint-dist (discrete-dist '(0.0 0.1 0.5 0.9 1.0)))

(: random-omega-rect (-> (U Empty-Set Omega-Rect)))
(define (random-omega-rect)
  (cond [((random) . < . 0.1)  empty-set]
        [else
         (let loop ([n 4])
           (define r (random))
           (cond [(or (r . < . 0.4) (zero? n))  omega-leaf]
                 [else  (omega-rect-node (random-interval unit-endpoint-dist)
                                         (loop (- n 1))
                                         (loop (- n 1)))]))]))

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice omega-rect-subseteq?
                          omega-rect-join
                          omega-rect-intersect
                          empty-set
                          omega-leaf
                          random-omega-rect)))
