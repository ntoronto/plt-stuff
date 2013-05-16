#lang typed/racket/base

(require racket/list
         "types.rkt"
         "real-set.rkt"
         "null-set.rkt"
         "bool-set.rkt"
         "pair-set.rkt"
         "extremal-set.rkt"
         "union.rkt"
         "value.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Basic set ops

(define-singleton-type Different different)

(: basic-member? (Basic Value -> (U Different Boolean)))
(define (basic-member? A x)
  (cond [(and (real-set? A) (flonum? x))   (real-set-member? A x)]
        [(and (bool-set? A) (boolean? x))  (bool-set-member? A x)]
        [(and (null-set? A) (null? x))     (null-set-member? A x)]
        [(and (pair-set? A) (pair? x))     (pair-set-member? A x)]
        [else  different]))

(: basic-complement (case-> (Nonextremal-Basic -> Nonextremal-Basic)
                            (Nonfull-Basic -> Nonempty-Basic)
                            (Nonempty-Basic -> Nonfull-Basic)
                            (Basic -> Basic)))
(define (basic-complement A)
  (cond [(real-set? A)  (real-set-complement A)]
        [(bool-set? A)  (bool-set-complement A)]
        [(null-set? A)  (null-set-complement A)]
        [else           (pair-set-complement A)]))

(: basic-subtract (case-> (Full-Basic Nonextremal-Basic -> (U Different Nonextremal-Basic))
                          (Full-Basic Nonfull-Basic -> (U Different Nonempty-Basic))
                          (Basic Nonempty-Basic -> (U Different Nonfull-Basic))
                          (Basic Basic -> (U Different Basic))))
(define (basic-subtract A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-subtract A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-subtract A B)]
        [(and (null-set? A) (null-set? B))  (null-set-subtract A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-subtract A B)]
        [else  different]))

(: basic-intersect (case-> (Nonfull-Basic Basic -> (U Different Nonfull-Basic))
                           (Basic Nonfull-Basic -> (U Different Nonfull-Basic))
                           (Basic Basic -> (U Different Basic))))
(define (basic-intersect A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-intersect A B)]
        [(and (null-set? A) (null-set? B))  (null-set-intersect A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-intersect A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-intersect A B)]
        [else  different]))

(: basic-union (case-> (Nonempty-Basic Basic -> (U Different Nonempty-Basic))
                       (Basic Nonempty-Basic -> (U Different Nonempty-Basic))
                       (Basic Basic -> (U Different Basic))))
(define (basic-union A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-union A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-union A B)]
        [(and (null-set? A) (null-set? B))  (null-set-union A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-union A B)]
        [else  different]))

;; ===================================================================================================
;; Membership

(: set-member? (Set Value -> Boolean))
(define (set-member? A x)
  (cond [(empty-set? A)   #f]
        [(universe? A)    #t]
        [(bot-set? A)
         (cond [(bot-basic? A)  (bot-basic-member? A x)]
               [(bot-tagged? A)  (bot-tagged-member? A x)]
               [else  (bot-union-member? A x)])]
        [else
         (cond [(top-basic? A)  (top-basic-member? A x)]
               [(top-tagged? A)  (top-tagged-member? A x)]
               [else  (top-union-member? A x)])]))

(: bot-basic-member? (Bot-Basic Value -> Boolean))
(define (bot-basic-member? A x)
  (define res (basic-member? A x))
  (if (different? res) #f res))

(: top-basic-member? (Top-Basic Value -> Boolean))
(define (top-basic-member? A x)
  (define Asub (top-basic-set A))
  (define res (basic-member? Asub x))
  (if (different? res) #t res))

(: bot-tagged-member? (Bot-Tagged Value -> Boolean))
(define (bot-tagged-member? A x)
  (and (tagged-value? x)
       (eq? (bot-tagged-tag A) (tagged-value-tag x))
       (set-member? (bot-tagged-set A) (tagged-value-value x))))

(: top-tagged-member? (Top-Tagged Value -> Boolean))
(define (top-tagged-member? A x)
  (or (not (tagged-value? x))
      (not (eq? (top-tagged-tag A) (tagged-value-tag x)))
      (set-member? (top-tagged-set A) (tagged-value-value x))))

(: bot-union-member? (Bot-Union Value -> Boolean))
(define (bot-union-member? A x)
  (set-member? (bot-union-ref A (value-tag x)) x))

(: top-union-member? (Top-Union Value -> Boolean))
(define (top-union-member? A x)
  (set-member? (top-union-ref A (value-tag x)) x))

;; ===================================================================================================
;; Union

(: set-union (case-> (Set Nonempty-Set -> Nonempty-Set)
                     (Nonempty-Set Set -> Nonempty-Set)
                     (Set Set -> Set)))
(define (set-union A B)
  (cond
    [(empty-set? A)  B]
    [(empty-set? B)  A]
    [(eq? A B)  B]
    [(universe? A)   A]
    [(universe? B)   B]
    [(bot-set? A)
     (if (bot-set? B)
         (if (and (bot-entry? A) (bot-entry? B)) (bot-bot-entry-union A B) (bot-bot-union A B))
         (if (and (bot-entry? A) (top-entry? B)) (top-bot-entry-union B A) (top-bot-union B A)))]
    [else
     (if (bot-set? B)
         (if (and (top-entry? A) (bot-entry? B)) (top-bot-entry-union A B) (top-bot-union A B))
         (if (and (top-entry? A) (top-entry? B)) (top-top-entry-union B A) (top-top-union B A)))]))

(: bot-bot-union ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union) -> Nonempty-Set))
(define (bot-bot-union A B)
  (for/fold ([A A]) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (define Dsub (if (empty-set? Asub) Bsub (bot-bot-entry-union Asub Bsub)))
    (bot-union-add A Dsub)))

(: top-bot-union ((U Top-Entry Top-Union) (U Bot-Entry Bot-Union) -> Nonempty-Set))
(define (top-bot-union A B)
  (for/fold: ([A : (U Universe Top-Entry Top-Union)  A]) ([Asub  (in-list (top-union-sets A))])
    (define a-tag (top-tag Asub))
    (define Bsub (bot-union-ref B a-tag))
    (define Dsub (if (empty-set? Bsub) Asub (top-bot-entry-union Asub Bsub)))
    (if (universe? Dsub) (top-union-remove A a-tag) (top-union-add A Dsub))))

(: top-top-union ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> Nonempty-Set))
(define (top-top-union A B)
  (for/fold: ([C : (U Universe Top-Entry Top-Union)  universe]) ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Dsub (if (universe? Asub) universe (top-top-entry-union Asub Bsub)))
    (if (universe? Dsub) C (top-union-add C Dsub))))

(: bot-bot-entry-union (Bot-Entry Bot-Entry -> (U Bot-Entry Bot-Union)))
(define (bot-bot-entry-union A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))  (bot-bot-basic-union A B)]
        [(and (bot-tagged? A) (bot-tagged? B)  (bot-bot-tagged-union A B))]
        [else  (bot-union A B)]))

(: top-bot-entry-union (Top-Entry Bot-Entry -> (U Top-Entry Top-Union Universe)))
(define (top-bot-entry-union A B)
  (cond [(and (top-basic? A)  (bot-basic? B))   (top-bot-basic-union A B)]
        [(and (top-tagged? A) (bot-tagged? B))  (top-bot-tagged-union A B)]
        [else  A]))

(: top-top-entry-union (Top-Entry Top-Entry -> (U Top-Entry Top-Union Universe)))
(define (top-top-entry-union A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-union A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-union A B)]
        [else  universe]))

(: bot-bot-basic-union (Bot-Basic Bot-Basic -> (U Bot-Basic Bot-Union)))
(define (bot-bot-basic-union A B)
  (define C (basic-union A B))
  (cond [(different? C)  (bot-union A B)]
        [(eq? C A)  A]
        [(eq? C B)  B]
        [else  C #;(bot-basic C)]))

(: top-bot-basic-union (Top-Basic Bot-Basic -> (U Top-Basic Top-Union Universe)))
(define (top-bot-basic-union A B)
  (define Asub (top-basic-set A))
  (define Csub (basic-union Asub B))
  (cond [(different? Csub)  A]
        [(eq? Csub Asub)  A]
        [else  (top-basic Csub)]))

(: top-top-basic-union (Top-Basic Top-Basic -> (U Top-Basic Top-Union Universe)))
(define (top-top-basic-union A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define Csub (basic-union Asub Bsub))
  (cond [(different? Csub)  universe]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-basic Csub)]))

(: bot-bot-tagged-union (Bot-Tagged Bot-Tagged -> (U Bot-Tagged Bot-Union)))
(define (bot-bot-tagged-union A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-union Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (bot-tagged a-tag Csub)])]
        [else  (bot-union A B)]))

(: top-bot-tagged-union (Top-Tagged Bot-Tagged -> (U Top-Tagged Top-Union Universe)))
(define (top-bot-tagged-union A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-union Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [else  (top-tagged a-tag Csub)])]
        [else  A]))

(: top-top-tagged-union (Top-Tagged Top-Tagged -> (U Top-Tagged Top-Union Universe)))
(define (top-top-tagged-union A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-union Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (top-tagged a-tag Csub)])]
        [else  universe]))

;; ===================================================================================================
;; Intersection

(: set-intersect (case-> (Set Nonfull-Set -> Nonfull-Set)
                         (Nonfull-Set Set -> Nonfull-Set)
                         (Set Set -> Set)))
(define (set-intersect A B)
  (cond [(universe? A)   B]
        [(universe? B)   A]
        [(eq? A B)  B]
        [(empty-set? A)  A]
        [(empty-set? B)  B]
        [(bot-set? A)
         (if (bot-set? B)
             (cond [(and (bot-entry? A) (bot-entry? B))  (bot-bot-entry-intersect A B)]
                   [else  (bot-bot-intersect A B)])
             (cond [(and (bot-entry? A) (top-entry? B))  (bot-top-entry-intersect A B)]
                   [else  (bot-top-intersect A B)]))]
        [else
         (if (bot-set? B)
             (cond [(and (top-entry? A) (bot-entry? B))  (bot-top-entry-intersect B A)]
                   [else  (bot-top-intersect B A)])
             (cond [(and (top-entry? A) (top-entry? B))  (top-top-entry-intersect A B)]
                   [else  (top-top-intersect A B)]))]))

(: bot-bot-intersect ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                                              -> (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-bot-intersect A B)
  (for/fold: ([C : (U Empty-Set Bot-Entry Bot-Union)  empty-set]
              ) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (define Dsub (if (empty-set? Asub) empty-set (bot-bot-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) C (bot-union-add C Dsub))))

(: bot-top-intersect ((U Bot-Entry Bot-Union) (U Top-Entry Top-Union)
                                              -> (U Bot-Entry Bot-Union Empty-Set)))
(define (bot-top-intersect A B)
  (for/fold: ([A : (U Empty-Set Bot-Entry Bot-Union)  A]) ([Asub  (in-list (bot-union-sets A))])
    (define a-tag (bot-tag Asub))
    (define Bsub (top-union-ref B a-tag))
    (define Dsub (if (universe? Bsub) Asub (bot-top-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) (bot-union-remove A a-tag) (bot-union-add A Dsub))))

(: top-top-intersect ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> (U Top-Entry Top-Union)))
(define (top-top-intersect A B)
  (for/fold ([A A]) ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Dsub (if (universe? Asub) Bsub (top-top-entry-intersect Asub Bsub)))
    (top-union-add A Dsub)))

(: bot-bot-entry-intersect (Bot-Entry Bot-Entry -> (U Bot-Entry Empty-Set)))
(define (bot-bot-entry-intersect A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-intersect A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-intersect A B)]
        [else  empty-set]))

(: bot-top-entry-intersect (Bot-Entry Top-Entry -> (U Bot-Entry Empty-Set)))
(define (bot-top-entry-intersect A B)
  (cond [(and (bot-basic? A)  (top-basic? B))   (bot-top-basic-intersect A B)]
        [(and (bot-tagged? A) (top-tagged? B))  (bot-top-tagged-intersect A B)]
        [else  A]))

(: top-top-entry-intersect (Top-Entry Top-Entry -> (U Top-Entry Top-Union)))
(define (top-top-entry-intersect A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-intersect A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-intersect A B)]
        [else  (top-union A B)]))

(: bot-bot-basic-intersect (Bot-Basic Bot-Basic -> (U Bot-Basic Empty-Set)))
(define (bot-bot-basic-intersect A B)
  (define C (basic-intersect A B))
  (cond [(different? C)  empty-set]
        [else  (bot-basic C)]))

(: bot-top-basic-intersect (Bot-Basic Top-Basic -> (U Bot-Basic Empty-Set)))
(define (bot-top-basic-intersect A B)
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect A Bsub))
  (cond [(different? Csub)  A]
        [else  (bot-basic Csub)]))

(: top-top-basic-intersect (Top-Basic Top-Basic -> (U Top-Basic Top-Union)))
(define (top-top-basic-intersect A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect Asub Bsub))
  (cond [(different? Csub)  (top-union A B)]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-basic Csub)]))

(: bot-bot-tagged-intersect (Bot-Tagged Bot-Tagged -> (U Bot-Tagged Empty-Set)))
(define (bot-bot-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (bot-tagged a-tag Csub)])]
        [else  empty-set]))

(: bot-top-tagged-intersect (Bot-Tagged Top-Tagged -> (U Bot-Tagged Empty-Set)))
(define (bot-top-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [else  (bot-tagged a-tag Csub)])]
        [else  A]))

(: top-top-tagged-intersect (Top-Tagged Top-Tagged -> (U Top-Tagged Top-Union)))
(define (top-top-tagged-intersect A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (top-tagged a-tag Csub)])]
        [else  (top-union A B)]))

;; ===================================================================================================
;; Complement

(: set-complement (case-> (Nonextremal-Set -> Nonextremal-Set)
                          (Nonfull-Set -> Nonempty-Set)
                          (Nonempty-Set -> Nonfull-Set)
                          (Set -> Set)))
(define (set-complement B)
  (cond [(empty-set? B)  universe]
        [(universe? B)   empty-set]
        [(bot-set? B)
         (cond [(bot-entry? B)  (bot-entry-complement B)]
               [else  (assert (bot-complement B) not-universe?)])]
        [else
         (cond [(top-entry? B)  (top-entry-complement B)]
               [else  (assert (top-complement B) not-empty-set?)])]))

(: bot-entry-complement (Bot-Entry -> Top-Entry))
(define (bot-entry-complement A)
  (cond [(bot-basic? A)  (bot-basic-complement A)]
        [else  (bot-tagged-complement A)]))

(: bot-basic-complement (Bot-Basic -> Top-Basic))
(define (bot-basic-complement A)
  (top-basic (basic-complement A)))

(: bot-tagged-complement (Bot-Tagged -> Top-Tagged))
(define (bot-tagged-complement A)
  (top-tagged (bot-tagged-tag A)
              (set-complement (bot-tagged-set A))))

(: top-entry-complement (Top-Entry -> Bot-Entry))
(define (top-entry-complement A)
  (cond [(top-basic? A)  (top-basic-complement A)]
        [else  (top-tagged-complement A)]))

(: top-basic-complement (Top-Basic -> Bot-Basic))
(define (top-basic-complement A)
  (basic-complement (top-basic-set A)))

(: top-tagged-complement (Top-Tagged -> Bot-Tagged))
(define (top-tagged-complement A)
  (bot-tagged (top-tagged-tag A)
              (set-complement (top-tagged-set A))))

(: bot-complement (Bot-Union -> (U Top-Entry Top-Union Universe)))
(define (bot-complement B)
  (for/fold: ([C : (U Top-Entry Top-Union Universe)  universe]
              ) ([Bsub  (in-list (bot-union-sets B))])
    (top-union-add C (bot-entry-complement Bsub))))

(: top-complement (Top-Union -> (U Bot-Entry Bot-Union Empty-Set)))
(define (top-complement B)
  (for/fold: ([C : (U Bot-Entry Bot-Union Empty-Set)  empty-set]
              ) ([Bsub  (in-list (top-union-sets B))])
    (bot-union-add C (top-entry-complement Bsub))))

;; ===================================================================================================
;; Difference

(: set-subtract (case-> (Universe Nonextremal-Set -> Nonextremal-Set)
                        (Universe Nonfull-Set -> Nonempty-Set)
                        (Set Nonempty-Set -> Nonfull-Set)
                        (Set Set -> Set)))
(define (set-subtract A B)
  (cond [(empty-set? B)  A]
        [(empty-set? A)  empty-set]
        [(universe? B)   empty-set]
        [(universe? A)   (set-complement B)]
        [(eq? A B)  empty-set]
        [(bot-set? A)
         (if (bot-set? B)
             (cond [(and (bot-entry? A) (bot-entry? B))  (bot-bot-entry-subtract A B)]
                   [else  (bot-bot-subtract A B)])
             (cond [(and (bot-entry? A) (top-entry? B))  (bot-top-entry-subtract A B)]
                   [else  (bot-top-subtract A B)]))]
        [else
         (if (bot-set? B)
             (cond [(and (top-entry? A) (bot-entry? B))  (top-bot-entry-subtract A B)]
                   [else  (top-bot-subtract A B)])
             (cond [(and (top-entry? A) (top-entry? B))  (top-top-entry-subtract A B)]
                   [else  (top-top-subtract A B)]))]))

(: bot-bot-subtract ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                                             -> (U Bot-Entry Bot-Union Empty-Set)))
(define (bot-bot-subtract A B)
  (for/fold: ([A : (U Bot-Entry Bot-Union Empty-Set)  A]) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (cond [(empty-set? Asub)  A]
          [else  (define Csub (bot-bot-entry-subtract Asub Bsub))
                 (cond [(empty-set? Csub)  (bot-union-remove A b-tag)]
                       [else  (bot-union-add A Csub)])])))

(: bot-top-subtract ((U Bot-Entry Bot-Union) (U Top-Entry Top-Union)
                                             -> (U Bot-Entry Bot-Union Empty-Set)))
(define (bot-top-subtract A B)
  (for/fold: ([C : (U Bot-Entry Bot-Union Empty-Set)  empty-set]
              ) ([Bsub  (in-list (top-union-sets B))])
    (define Asub (bot-union-ref A (top-tag Bsub)))
    (cond [(empty-set? Asub)  C]
          [else  (define D (bot-top-entry-subtract Asub Bsub))
                 (if (empty-set? D) C (bot-union-add C D))])))

(: top-bot-subtract ((U Top-Entry Top-Union) (U Bot-Entry Bot-Union) -> (U Top-Entry Top-Union)))
(define (top-bot-subtract A B)
  (for/fold ([A A]) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Csub (if (universe? Asub)
                     (bot-entry-complement Bsub)
                     (top-bot-entry-subtract Asub Bsub)))
    (top-union-add A Csub)))

(: top-top-subtract ((U Top-Entry Top-Union) (U Top-Entry Top-Union)
                                             -> (U Bot-Entry Bot-Union Empty-Set)))
(define (top-top-subtract A B)
  (for/fold: ([C : (U Bot-Entry Bot-Union Empty-Set)  empty-set]
              ) ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Dsub (if (universe? Asub)
                     (top-entry-complement Bsub)
                     (top-top-entry-subtract Asub Bsub)))
    (cond [(empty-set? Dsub)  C]
          [else  (bot-union-add C Dsub)])))

(: bot-bot-entry-subtract (Bot-Entry Bot-Entry -> (U Bot-Entry Empty-Set)))
(define (bot-bot-entry-subtract A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-subtract A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-subtract A B)]
        [else  A]))

(: bot-top-entry-subtract (Bot-Entry Top-Entry -> (U Bot-Entry Empty-Set)))
(define (bot-top-entry-subtract A B)
  (cond [(and (bot-basic? A)  (top-basic? B))   (bot-top-basic-subtract A B)]
        [(and (bot-tagged? A) (top-tagged? B))  (bot-top-tagged-subtract A B)]
        [else  empty-set]))

(: top-bot-entry-subtract (Top-Entry Bot-Entry -> (U Top-Entry Top-Union)))
(define (top-bot-entry-subtract A B)
  (cond [(and (top-basic? A)  (bot-basic? B))   (top-bot-basic-subtract A B)]
        [(and (top-tagged? A) (bot-tagged? B))  (top-bot-tagged-subtract A B)]
        [else  (top-union A (bot-entry-complement B))]))

(: top-top-entry-subtract (Top-Entry Top-Entry -> (U Bot-Entry Empty-Set)))
(define (top-top-entry-subtract A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-subtract A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-subtract A B)]
        [else  (top-entry-complement B)]))

(: bot-bot-basic-subtract (Bot-Basic Bot-Basic -> (U Bot-Basic Empty-Set)))
(define (bot-bot-basic-subtract A B)
  (define C (basic-subtract A B))
  (cond [(different? C)  A]
        [else  (bot-basic C)]))

(: bot-top-basic-subtract (Bot-Basic Top-Basic -> (U Bot-Basic Empty-Set)))
(define (bot-top-basic-subtract A B)
  (define Bsub (top-basic-set B))
  (define Csub (basic-subtract A Bsub))
  (cond [(different? Csub)  empty-set]
        [else  (bot-basic Csub)]))

(: top-bot-basic-subtract (Top-Basic Bot-Basic -> (U Top-Basic Top-Union)))
(define (top-bot-basic-subtract A B)
  (define Asub (top-basic-set A))
  (define Csub (basic-subtract Asub B))
  (cond [(different? Csub)  (top-union A (bot-basic-complement B))]
        [else  (top-basic Csub)]))

(: top-top-basic-subtract (Top-Basic Top-Basic -> (U Bot-Basic Empty-Set)))
(define (top-top-basic-subtract A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define Csub (basic-subtract Asub Bsub))
  (cond [(different? Csub)  (top-basic-complement B)]
        [else  (bot-basic Csub)]))

(: bot-bot-tagged-subtract (Bot-Tagged Bot-Tagged -> (U Bot-Tagged Empty-Set)))
(define (bot-bot-tagged-subtract A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-subtract Asub Bsub))
         (bot-tagged a-tag Csub)]
        [else  A]))

(: bot-top-tagged-subtract (Bot-Tagged Top-Tagged -> (U Bot-Tagged Empty-Set)))
(define (bot-top-tagged-subtract A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-subtract Asub Bsub))
         (bot-tagged a-tag Csub)]
        [else  empty-set]))

(: top-bot-tagged-subtract (Top-Tagged Bot-Tagged -> (U Top-Tagged Top-Union)))
(define (top-bot-tagged-subtract A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-subtract Asub Bsub))
         (top-tagged a-tag Csub)]
        [else
         (top-union A (bot-tagged-complement B))]))

(: top-top-tagged-subtract (Top-Tagged Top-Tagged -> (U Bot-Tagged Empty-Set)))
(define (top-top-tagged-subtract A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-subtract Asub Bsub))
         (bot-tagged a-tag Csub)]
        [else
         (top-tagged-complement B)]))

;; ===================================================================================================

(define set-ops
  ((inst set-sig Nonextremal-Set Universe Empty-Set Value)
   set-member?
   universe?
   empty-set?
   universe
   empty-set
   set-complement
   set-subtract
   set-intersect
   set-union))

(define pair-set
  (make-pair-set Nonextremal-Set Universe Empty-Set Value set-ops))

(define pair-set-member?
  ((inst make-pair-set-member? Nonextremal-Set Universe Empty-Set Value) set-ops))

(define pair-set-complement
  ((inst make-pair-set-complement Nonextremal-Set Universe Empty-Set Value) set-ops))

(define pair-set-subtract
  ((inst make-pair-set-subtract Nonextremal-Set Universe Empty-Set Value) set-ops))

(define pair-set-intersect
  ((inst make-pair-set-intersect Nonextremal-Set Universe Empty-Set Value) set-ops))

(define pair-set-union
  ((inst make-pair-set-union Nonextremal-Set Universe Empty-Set Value) set-ops))

(define pair-set-map
  ((inst make-pair-set-map Nonextremal-Set Universe Empty-Set Value) set-ops))
