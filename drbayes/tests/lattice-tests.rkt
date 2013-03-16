#lang typed/racket

;; Verify that rectangular sets and operations on them comprise bounded lattices w.r.t. the subset
;; partial order, using randomized testing

;; Also implicitly checks that sets have a canonical form; i.e. that two instances represent the
;; same set iff they are `equal?'

(require typed/rackunit
         math/distributions
         "../private/set.rkt")

(define-syntax-rule (implies a b) (or (not a) b))

(: flip (All (A B C) ((A B -> C) -> (B A -> C))))
(define ((flip f) x y) (f y x))

;; Using this is about 1000x faster than using `check-true' directly, mostly because it doesn't have
;; to construct the message unless there's a failure
(define-syntax-rule (check-prop expr msg)
  (if expr (void) (check-true expr msg)))

;; ===================================================================================================
;; Bounded lattice property checks

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

;; All bounded lattice properties

(: check-bounded-lattice (All (T) ((T T -> Boolean) (T T -> T) (T T -> T) T T (-> T) -> Any)))
(define (check-bounded-lattice lte? join meet bot top random-set)
  (define A (random-set))
  (define B (random-set))
  (define C (random-set))
  (define D (random-set))
  
  ;(printf "A = ~v~n" A)
  ;(printf "B = ~v~n" B)
  ;(printf "C = ~v~n" C)
  ;(printf "D = ~v~n~n" D)
  
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
;; Checks for lattices defined by membership

(: check-member (All (T X) ((T X -> Boolean) T X -> Any)))
(define (check-member member? A x)
  (check-prop (member? A x)
              (format "~a: membership failed on ~v ~v" member? A x)))

(: check-subseteq (All (T X) ((T X -> Boolean) (T T -> Boolean) T X T X -> Any)))
(define (check-subseteq member? subseteq? A x B y)
  (when (subseteq? A B)
    (check-prop (implies (member? A x) (member? B x))
                (format "~a ~a: subseteq membership failed on ~v ~v ~v" member? subseteq? A B x))
    (check-prop (implies (not (member? B x)) (not (member? A x)))
                (format "~a ~a: subseteq non-membership failed on ~v ~v ~v" member? subseteq? A B x))
    (check-prop (implies (member? A y) (member? B y))
                (format "~a ~a: subseteq membership failed on ~v ~v ~v" member? subseteq? A B y))
    (check-prop (implies (not (member? B y)) (not (member? A y)))
                (format "~a ~a: subseteq non-membership failed on ~v ~v ~v" member? subseteq? A B y))
    ))

(: check-join (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-join member? join A x B y)
  (define C (join A B))
  (check-prop (implies (or (member? A x) (member? B x)) (member? C x))
              (format "~a ~a: join membership failed on ~v ~v ~v" member? join A B x))
  (check-prop (implies (or (member? A y) (member? B y)) (member? C y))
              (format "~a ~a: join membership failed on ~v ~v ~v" member? join A B y)))

(: check-meet (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-meet member? meet A x B y)
  (define C (meet A B))
  (check-prop (implies (and (member? A x) (member? B x)) (member? C x))
              (format "~a ~a: meet membership failed on ~v ~v ~v ~v" member? meet A B x))
  (check-prop (implies (and (member? A y) (member? B y)) (member? C y))
              (format "~a ~a: meet membership failed on ~v ~v ~v ~v" member? meet A B y)))

;; All membership lattice properties

(: check-membership-lattice (All (T X) ((T -> Boolean)
                                        (T X -> Boolean)
                                        (T T -> Boolean)
                                        (T T -> T)
                                        (T T -> T)
                                        (-> T)
                                        (T -> X) -> Any)))
(define (check-membership-lattice empty? member? subseteq? join meet random-set random-member)
  (define A (random-set))
  (define B (random-set))
  
  ;(printf "A = ~v~n" A)
  ;(printf "B = ~v~n" B)

   (when (and (not (empty? A)) (not (empty? B)))
     (define x (random-member A))
     (define y (random-member B))
     
     ;(printf "x = ~v~n" x)
     ;(printf "y = ~v~n" y)
     
     (check-member member? A x)
     (check-member member? B y)
     
     (check-subseteq member? subseteq? A x B y)
     
     (check-join member? join A x B y)
     (check-meet member? meet A x B y)))

;; ===================================================================================================
;; Random intervals

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

;; ===================================================================================================
;; Random Set generation

(: random-tagged-interval (-> Interval))
(define (random-tagged-interval)
  (random-interval real-endpoint-dist))

(: random-pair-rect (-> Pair-Rect))
(define (random-pair-rect)
  (define A (set-pair (random-set) (random-set)))
  (if (empty-set? A) (random-pair-rect) A))

(: random-boolean-rect (-> Boolean-Rect))
(define (random-boolean-rect)
  (define r (random))
  (cond [(r . < . #i1/3)  trues]
        [(r . < . #i2/3)  falses]
        [else  booleans]))

(define a-tag (make-set-tag 'a))
(define b-tag (make-set-tag 'b))

(: random-rect (-> Rect))
(define (random-rect)
  (define r (random))
  (cond [(r . < . #i1/4)  (random-tagged-interval)]
        [(r . < . #i2/4)  null-set]
        [(r . < . #i3/4)  (random-pair-rect)]
        [else             (random-boolean-rect)]))

(: random-top-rect (-> Top-Rect))
(define (random-top-rect)
  (define A (random-rect))
  (if ((random) . < . #i1/5)
      (top-rect (rect-tag A) empty-set)
      (top-rect (rect-tag A) A)))

(: random-bot-set (-> Bot-Set))
(define (random-bot-set)
  (define A (bot-set (if ((random) . < . 0.5) a-tag b-tag)
                     (random-set)))
  (if (empty-set? A) (random-bot-set) A))

(: random-top-set (-> Top-Set))
(define (random-top-set)
  (define A (top-set (if ((random) . < . 0.5) a-tag b-tag)
                     (random-set)))
  (if (universe? A) (random-top-set) A))

(: random-bot-union (-> Bot-Union))
(define (random-bot-union)
  (define A
    (for/fold: ([A : (U Empty-Set Bot-Entry Bot-Union)  empty-set]) ([_  (in-range 3)])
      (if ((random) . < . #i2/3)
          (bot-union-add A (random-rect))
          (bot-union-add A (random-bot-set)))))
  (if (bot-union? A) A (random-bot-union)))

(: random-top-union (-> Top-Union))
(define (random-top-union)
  (define A
    (for/fold: ([A : (U Universe Top-Entry Top-Union)  universe]) ([_  (in-range 3)])
      (if ((random) . < . #i2/3)
          (top-union-add A (random-top-rect))
          (top-union-add A (random-top-set)))))
  (if (top-union? A) A (random-top-union)))

(: set-depth (Parameterof Natural))
(define set-depth (make-parameter 0))

(: random-set (-> Set))
(define (random-set)
  (cond [((set-depth) . > . 3)
         (if ((random) . < . 0.5)
             empty-set
             universe)]
        [else
         (parameterize ([set-depth  (+ 1 (set-depth))])
           (define r (random))
           (cond [(r . < . #i1/8)  empty-set]
                 [(r . < . #i2/8)  (random-rect)]
                 [(r . < . #i3/8)  (random-bot-set)]
                 [(r . < . #i4/8)  (random-bot-union)]
                 [(r . < . #i5/8)  universe]
                 [(r . < . #i6/8)  (random-top-rect)]
                 [(r . < . #i7/8)  (random-top-set)]
                 [else             (random-top-union)]))]))

;; ===================================================================================================
;; Random Value generation

(: random-element (All (A) ((Listof A) -> A)))
(define (random-element xs)
  (list-ref xs (random (length xs))))

(define random-reals '(-3.0 -2.0 -1.5 -1.0 -0.5 -0.0 0.0 0.5 1.0 1.5 2.0 3.0))

(: random-real (Interval -> Flonum))
(define (random-real I)
  (random-element (filter (λ: ([x : Flonum]) (interval-member? I x)) random-reals)))

(: random-rect-member (Rect -> Value))
(define (random-rect-member A)
  (cond [(interval? A)  (random-real A)]
        [(null-rect? A)   null]
        [(pair-rect? A)
         (match-define (pair-rect A1 A2) A)
         (cons (random-set-member A1) (random-set-member A2))]
        [(boolean-rect? A)
         (cond [(eq? A booleans)  (if ((random) . < . 0.5) #t #f)]
               [(eq? A trues)     #t]
               [(eq? A falses)    #f]
               [else  (error 'random-rect-member "bad boolean set ~e" A)])]))

(: random-bot-set-member (Bot-Set -> Value))
(define (random-bot-set-member A)
  (match-let ([(Tagged tag A)  A])
    (tagged tag (random-set-member A))))

(: random-bot-union-member (Bot-Union -> Value))
(define (random-bot-union-member A)
  (define As (bot-union-sets A))
  (random-set-member (random-element As)))

(define tags (list real-tag null-tag pair-tag boolean-tag a-tag b-tag))

(: random-top-rect-member (Top-Rect -> Value))
(define (random-top-rect-member A)
  (match-let ([(Tagged t A)  A])
    (let loop ()
      (define tag (random-element tags))
      (cond [(eq? tag t)  (if (empty-set? A) (loop) (random-rect-member A))]
            [(eq? tag a-tag)  (tagged a-tag (random-universe-member))]
            [(eq? tag b-tag)  (tagged b-tag (random-universe-member))]
            [(eq? tag real-tag)  (random-real real-interval)]
            [(eq? tag null-tag)  null]
            [(eq? tag pair-tag)  (random-set-member all-pairs)]
            [(eq? tag boolean-tag)  (if ((random) . < . 0.5) #t #f)]
            [else  (loop)]))))

(: random-top-set-member (Top-Set -> Value))
(define (random-top-set-member A)
  (match-let ([(Tagged t A)  A])
    (let loop ()
      (define tag (random-element tags))
      (cond [(eq? tag t)  (if (empty-set? A) (loop) (random-bot-set-member (bot-set t A)))]
            [(eq? tag a-tag)  (tagged a-tag (random-universe-member))]
            [(eq? tag b-tag)  (tagged b-tag (random-universe-member))]
            [(eq? tag real-tag)  (random-real real-interval)]
            [(eq? tag null-tag)  null]
            [(eq? tag pair-tag)  (random-set-member all-pairs)]
            [(eq? tag boolean-tag)  (if ((random) . < . 0.5) #t #f)]
            [else  (loop)]))))

(: random-top-union-member (Top-Union -> Value))
(define (random-top-union-member A)
  (define x (random-universe-member))
  (if (set-member? A x) x (random-top-union-member A)))

(: random-universe-member (-> Value))
(define (random-universe-member)
  (define A (random-set))
  (if (or (empty-set? A) (universe? A)) (random-universe-member) (random-set-member A)))

(: random-set-member (Set -> Value))
(define (random-set-member A)
  (cond [(empty-set? A)  (raise-argument-error 'random-set-member "Nonempty-Set" A)]
        [(universe? A)   (random-universe-member)]
        [(rect? A)       (random-rect-member A)]
        [(bot-set? A)    (random-bot-set-member A)]
        [(bot-union? A)  (random-bot-union-member A)]
        [(top-rect? A)   (random-top-rect-member A)]
        [(top-set? A)    (random-top-set-member A)]
        [(top-union? A)  (random-top-union-member A)]))

;; ===================================================================================================
;; Random Omega-Rect generation

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

;; ===================================================================================================
;; Tests

(printf "Bounded lattice tests for Set...~n")
(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice set-subseteq?
                          set-join
                          set-intersect
                          empty-set
                          universe
                          random-set)))
(newline)

(printf "Membership lattice tests for Set...~n")
(time
 (for: ([_  (in-range 100000)])
   ((inst check-membership-lattice Set Value) empty-set?
                                              set-member?
                                              set-subseteq?
                                              set-join
                                              set-intersect
                                              random-set
                                              random-set-member)))
(newline)

(printf "Bounded lattice tests for Omega-Rect...~n")
(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice omega-rect-subseteq?
                          omega-rect-join
                          omega-rect-intersect
                          empty-set
                          omega-leaf
                          random-omega-rect)))
(newline)

(printf "(TODO) Membership lattice tests for Omega-Rect...~n")
#;
(time
 (for: ([_  (in-range 100000)])
   ((inst check-membership-lattice Maybe-Omega-Rect Omega) empty-set?
                                                           omega-rect-member?
                                                           omega-rect-subseteq?
                                                           omega-rect-join
                                                           omega-rect-intersect
                                                           random-omega-rect
                                                           omega-rect-sample-point)))
(newline)
