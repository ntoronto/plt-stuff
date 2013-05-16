#lang typed/racket/base

(require racket/list
         racket/match
         "types.rkt"
         "real-set.rkt"
         "null-set.rkt"
         "bool-set.rkt"
         "pair-set.rkt"
         "extremal-set.rkt"
         "union.rkt"
         "value.rkt"
         "union-ops.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(: set-subseteq? (Set Set -> Boolean))
(define (set-subseteq? A B)
  (empty-set? (set-subtract A B)))

(: set-equal? (Set Set -> Boolean))
(define (set-equal? A B)
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

(: set-pair
   (case->
    (Nonempty-Set Nonempty-Set -> (U (Nonextremal-Pair-Rect Nonempty-Set Universe) Full-Pair-Set))
    (Set Set -> (U (Nonextremal-Pair-Rect Nonempty-Set Universe) Full-Pair-Set Empty-Set))))
(define (set-pair A B)
  (cond [(and (universe? A) (universe? B))   pairs]
        [(empty-set? A)  empty-set]
        [(empty-set? B)  empty-set]
        [else  ((inst Nonextremal-Pair-Rect Nonempty-Set Universe) A B)]))

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

#|
(: basic-untag-empty (Tag -> Empty-Basic))
(define (basic-untag-empty tag)
  (cond [(eq? tag real-tag)  empty-real-set]
        [(eq? tag bool-tag)  empty-bool-set]
        [(eq? tag null-tag)  empty-null-set]
        [(eq? tag pair-tag)  empty-pair-set]
        [else  (raise-argument-error 'basic-untag "basic Tag" tag)]))

(: basic-untag-full (Tag -> Full-Basic))
(define (basic-untag-full tag)
  (cond [(eq? tag real-tag)  reals]
        [(eq? tag bool-tag)  bools]
        [(eq? tag null-tag)  nulls]
        [(eq? tag pair-tag)  pairs]
        [else  (raise-argument-error 'basic-untag "basic Tag" tag)]))

(: basic-untag (Set Tag -> Basic))
(define (basic-untag A tag)
  (cond [(empty-set? A)   (basic-untag-empty tag)]
        [(universe? A)    (basic-untag-full tag)]
        [(bot-set? A)
         (cond [(bot-basic? A)   (if (eq? tag (basic-tag A))
                                     A
                                     (basic-untag-empty tag))]
               [(bot-tagged? A)  (basic-untag-empty tag)]
               [else             (basic-untag (bot-union-ref A tag) tag)])]
        [else
         (cond [(top-basic? A)   (if (eq? tag (top-basic-tag A))
                                     (top-basic-set A)
                                     (basic-untag-full tag))]
               [(top-tagged? A)  (basic-untag-full tag)]
               [else             (basic-untag (top-union-ref A tag) tag)])]))
|#

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

(: set-take-pairs (Set -> (Pair-Set Nonempty-Set Universe)))
(define set-take-pairs (make-set-take-basic pair-set? pair-tag empty-pair-set pairs))

;; ===================================================================================================
;; Pair ref and set

(: set-pair-ref (Set Pair-Index -> Set))
(define (set-pair-ref A j)
  (let ([A  (set-take-pairs A)])
    (cond [(pair-rect? A)  (pair-rect-ref A j)]
          [(pair-rect-list? A)
           (define As (pair-rect-list-elements A))
           (for/fold ([B  (pair-rect-ref (first As) j)]) ([A  (in-list (rest As))])
             (set-union B (pair-rect-ref A j)))]
          [else  empty-set])))

(: pair-rect-ref (Basic Pair-Index -> Set))
(define (pair-rect-ref A j)
  (match-define (pair-rect A1 A2) A)
  (cond [(eq? j 'fst)  A1]
        [(eq? j 'snd)  A2]
        [(zero? j)  A1]
        [else  (set-pair-ref A2 (- j 1))]))

(: universal-pair-set (Pair-Index Nonempty-Set -> Nonempty-Set))
(define (universal-pair-set j A)
  (cond [(eq? j 'fst)  (set-pair A universe)]
        [(eq? j 'snd)  (set-pair universe A)]
        [(zero? j)     (set-pair A universe)]
        [else  (set-pair universe (universal-pair-set (- j 1) A))]))
