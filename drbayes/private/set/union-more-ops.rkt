#lang typed/racket/base

(require racket/list
         racket/match
         "types.rkt"
         "real-set.rkt"
         "null-set.rkt"
         "bool-set.rkt"
         "extremal-set.rkt"
         "union.rkt"
         "value.rkt"
         "union-ops.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(: set-equal? (Set Set -> Boolean))
(define (set-equal? A B)
  (equal? A B)
  #;
  (and (set-subseteq? A B) (set-subseteq? B A)))

;; ===================================================================================================
;; Extra constructors

(: real-set (case-> (-> Full-Real-Set)
                    (Flonum Flonum -> (U Nonempty-Interval Empty-Set))
                    (Flonum Flonum Boolean Boolean -> (U Nonempty-Interval Empty-Set))))
(define real-set
  (case-lambda
    [()  reals]
    [(a b)
     (define A (interval a b))
     (if (empty-real-set? A) empty-set A)]
    [(a b a? b?)
     (define A (interval a b a? b?))
     (if (empty-real-set? A) empty-set A)]))

(: set-pair (case-> (Nonempty-Set Nonempty-Set -> Nonempty-Pair-Set)
                    (Set Set -> (U Empty-Set Nonempty-Pair-Set))))
(define (set-pair A B)
  (define C (pair-set A B))
  (if (empty-pair-set? C) empty-set C))

(: set-list (case-> (Nonempty-Set * -> Nonempty-Set)
                    (Set * -> Set)))
(define (set-list . As)
  (foldr set-pair nulls As))

(: set-list* (case-> (Nonempty-Set Nonempty-Set * -> Nonempty-Set)
                     (Set Set * -> Set)))
(define (set-list* A . As)
  (let loop ([A A] [As As])
    (cond [(empty? As)  A]
          [else  (set-pair A (loop (first As) (rest As)))])))

;; ===================================================================================================
;; Tagging and untagging

(: set-tag (case-> (Nonempty-Set Tag -> Bot-Tagged)
                   (Set Tag -> (U Bot-Tagged Empty-Set))))
(define (set-tag A tag)
  (bot-tagged tag A))

(: set-untag (Set Tag -> Set))
(define (set-untag A tag)
  (cond [(empty-set? A)   empty-set]
        [(universe? A)    universe]
        [(bot-basic? A)   empty-set]
        [(bot-tagged? A)  (if (eq? tag (bot-tagged-tag A)) (bot-tagged-set A) empty-set)]
        [(bot-union? A)   (set-untag (bot-union-ref A tag) tag)]
        [(top-basic? A)   universe]
        [(top-tagged? A)  (if (eq? tag (top-tagged-tag A)) (top-tagged-set A) universe)]
        [else             (set-untag (top-union-ref A tag) tag)]))

(define-syntax-rule (make-set-take-basic pred? tag empty full)
  (λ (A)
    (let loop ([A A])
      (cond [(empty-set? A)  empty]
            [(universe? A)   full]
            [(bot-set? A)
             (cond [(bot-basic? A)   (if (pred? A) A empty)]
                   [(bot-tagged? A)  empty]
                   [else             (loop (bot-union-ref A tag))])]
            [else
             (cond [(top-basic? A)   (define Asub (top-basic-set A))
                                     (if (pred? Asub) Asub full)]
                   [(top-tagged? A)  full]
                   [else             (loop (top-union-ref A tag))])]))))

(: set-take-reals (Set -> Real-Set))
(define set-take-reals (make-set-take-basic real-set? real-tag empty-real-set reals))

(: set-take-bools (Set -> Bool-Set))
(define set-take-bools (make-set-take-basic bool-set? bool-tag empty-bool-set bools))

(: set-take-nulls (Set -> Null-Set))
(define set-take-nulls (make-set-take-basic null-set? null-tag empty-null-set nulls))

(: set-take-pairs (Set -> Pair-Set))
(define set-take-pairs (make-set-take-basic pair-set? pair-tag empty-pair-set pairs))

;; ===================================================================================================
;; Pair ref and set

(: set-pair-ref (Set Pair-Index -> Set))
(define (set-pair-ref A j)
  (let ([A  (set-take-pairs A)])
    (cond [(empty-pair-set? A)  empty-set]
          [else
           (define-values (A1 A2) (cond [(pairs? A)  (values universe universe)]
                                        [else  (match-define (Nonextremal-Pair-Set A1 A2) A)
                                               (values A1 A2)]))
           (cond [(eq? j 'fst)  A1]
                 [(eq? j 'snd)  A2]
                 [(zero? j)     A1]
                 [else  (set-pair-ref A2 (- j 1))])])))

(: set-pair-restrict (Set Pair-Index Set -> Set))
(define (set-pair-restrict A j B)
  (let ([A  (set-take-pairs A)])
    (cond [(or (empty-pair-set? A) (empty-set? B))  empty-set]
          [else
           (define-values (A1 A2) (cond [(pairs? A)  (values universe universe)]
                                        [else  (match-define (Nonextremal-Pair-Set A1 A2) A)
                                               (values A1 A2)]))
           (cond [(eq? j 'fst)  (set-pair (set-intersect A1 B) A2)]
                 [(eq? j 'snd)  (set-pair A1 (set-intersect A2 B))]
                 [(zero? j)     (set-pair (set-intersect A1 B) A2)]
                 [else  (set-pair A1 (set-pair-restrict A2 (- j 1) B))])])))
