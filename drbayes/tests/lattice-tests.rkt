#lang typed/racket

;; Verify that rectangular sets and operations on them comprise bounded lattices w.r.t. the subset
;; partial order, using randomized testing

;; Also implicitly checks that sets have a canonical form; i.e. that two instances represent the
;; same set iff they are `equal?'

(require typed/rackunit
         math/distributions
         "../private/set.rkt"
         "rackunit-utils.rkt"
         "random-interval.rkt")

;; ===================================================================================================
;; Random Set generation

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
  (cond [(r . < . #i1/4)  (random-interval*)]
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

(: random-rect-member (Rect -> Value))
(define (random-rect-member A)
  (cond [(interval*? A)  (random-real A)]
        [(null-rect? A)  null]
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

(: omega-rect-node-subseteq? ((Omega-Node Interval*) (Omega-Node Interval*) -> Boolean))
(define (omega-rect-node-subseteq? A B)
  (match-define (Omega-Node I1 A1 A2) A)
  (match-define (Omega-Node I2 B1 B2) B)
  (and (interval*-subseteq? I1 I2)
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
