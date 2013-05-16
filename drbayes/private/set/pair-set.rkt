#lang typed/racket/base

#|
TODO: Look up self-balancing quadtrees to change set ops from O(n^2) to O(n*log(n))
|#

(require racket/list
         "types.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(struct: Base-Pair-Set Base-Bot-Basic () #:transparent)
(define pair-set? Base-Pair-Set?)

(define-singleton-type Empty-Pair-Set Base-Pair-Set empty-pair-set)
(define-singleton-type Full-Pair-Set Base-Pair-Set pairs)

(struct: (N F) Nonextremal-Pair-Rect Base-Pair-Set
  ([fst : (U N F)] [snd : (U N F)])
  #:transparent)

(define-type (Nonfull-Pair-Rect N F) (U (Nonextremal-Pair-Rect N F) Empty-Pair-Set))
(define-type (Nonempty-Pair-Rect N F) (U (Nonextremal-Pair-Rect N F) Full-Pair-Set))
(define-type (Pair-Rect N F) (U (Nonextremal-Pair-Rect N F) Full-Pair-Set Empty-Pair-Set))

(struct: (N F) Nonextremal-Pair-Rect-List Base-Pair-Set
  ([elements : (Listof+2 (Nonextremal-Pair-Rect N F))])
  #:transparent)

(define-type (Nonextremal-Pair-Set N F)
  (U (Nonextremal-Pair-Rect N F) (Nonextremal-Pair-Rect-List N F)))

(define-type (Nonfull-Pair-Set N F) (U (Nonextremal-Pair-Set N F) Empty-Pair-Set))
(define-type (Nonempty-Pair-Set N F) (U (Nonextremal-Pair-Set N F) Full-Pair-Set))
(define-type (Pair-Set N F) (U (Nonextremal-Pair-Set N F) Full-Pair-Set Empty-Pair-Set))

(: nonfull-pair-set->list
   (All (N F) (case-> ((Nonextremal-Pair-Set N F) -> (Listof+1 (Nonextremal-Pair-Rect N F)))
                      ((Nonfull-Pair-Set N F) -> (Listof (Nonextremal-Pair-Rect N F))))))
(define (nonfull-pair-set->list A)
  (cond [(empty-pair-set? A)  '()]
        [(Nonextremal-Pair-Rect? A)  (list A)]
        [else  (Nonextremal-Pair-Rect-List-elements A)]))

(: list->pair-set
   (All (N F) (case-> ((Listof+1 (Nonextremal-Pair-Rect N F)) -> (Nonextremal-Pair-Set N F))
                      ((Listof (Nonextremal-Pair-Rect N F)) -> (Nonfull-Pair-Set N F)))))
(define (list->pair-set Cs)
  (cond [(empty? Cs)  empty-pair-set]
        [(empty? (rest Cs))  (first Cs)]
        [else  (Nonextremal-Pair-Rect-List Cs)]))

;; ===================================================================================================
;; Operations on Pair-Rect

(struct: (N F E V) set-sig
  ([member? : ((U N F E) V -> Boolean)]
   [full? : (Any -> Boolean : F)]
   [empty? : (Any -> Boolean : E)]
   [full : F]
   [empty : E]
   [complement
    : (case->
       (N -> N)
       ((U N E) -> (U N F))
       ((U N F) -> (U N E))
       ((U N F E) -> (U N F E)))]
   [subtract
    : (case->
       (F N -> N)
       (F (U N E) -> (U N F))
       ((U N F E) (U N F) -> (U N E))
       ((U N F E) (U N F E) -> (U N F E)))]
   [intersect
    : (case->
       ((U N F E) (U N E) -> (U N E))
       ((U N E) (U N F E) -> (U N E))
       ((U N F E) (U N F E) -> (U N F E)))]
   [union
    : (case->
       ((U N F E) (U N F) -> (U N F))
       ((U N F) (U N F E) -> (U N F))
       ((U N F E) (U N F E) -> (U N F E)))])
  #:transparent)

;; This has to be a syntax rule so that the occurrence checks in the body have concrete types to work
;; with. Otherwise, TR can't exclude disjoint types in each occurrence because it doesn't know whether
;; they're disjoint; e.g. it can't tell that if (full? A) is true then (empty? A) is false.
(define-syntax-rule (make-pair-set N F E V ops)
  (let: ([ops : (set-sig N F E V)  ops]
         [full?  ((inst set-sig-full? N F E V) ops)]
         [empty?  ((inst set-sig-empty? N F E V) ops)])
    (ann (λ (A B)
           (cond [(and (full? A) (full? B))   pairs]
                 [(empty? A)  empty-pair-set]
                 [(empty? B)  empty-pair-set]
                 [else  ((inst Nonextremal-Pair-Rect N F) A B)]))
         (case-> ((U N F) N -> (Nonextremal-Pair-Rect N F))
                 (N (U N F) -> (Nonextremal-Pair-Rect N F))
                 ((U N F) (U N F) -> (Nonempty-Pair-Rect N F))
                 ((U N E) (U N F E) -> (Nonfull-Pair-Rect N F))
                 ((U N F E) (U N E) -> (Nonfull-Pair-Rect N F))
                 ((U N F E) (U N F E) -> (Pair-Rect N F))))))

(: make-pair-rect-complement
   (All (N F E V) ((set-sig N F E V)
                   -> ((Nonextremal-Pair-Rect N F) -> (Listof+1 (Nonextremal-Pair-Rect N F))))))
(define (make-pair-rect-complement ops)
  (define empty? (set-sig-empty? ops))
  (define full (set-sig-full ops))
  (define complement (set-sig-complement ops))
  (λ (B)
    (define B1 (Nonextremal-Pair-Rect-fst B))
    (define B2 (Nonextremal-Pair-Rect-snd B))
    (define C
      (let ([C1  (complement B1)])
        (if (empty? C1) empty-pair-set (Nonextremal-Pair-Rect C1 full))))
    (define D
      (let ([D2  (complement B2)])
        (if (empty? D2) empty-pair-set (Nonextremal-Pair-Rect B1 D2))))
    (if (empty-pair-set? C)
        (if (empty-pair-set? D)
            ;; Either B1 or B2 is nonfull, but we can't prove it
            (raise-argument-error 'pair-rect-complement "nonfull Nonextremal-Pair-Rect" B)
            (list D))
        (if (empty-pair-set? D) (list C) (list C D)))))

(: make-pair-rect-subtract
   (All (N F E V) ((set-sig N F E V)
                   -> ((Nonextremal-Pair-Rect N F) (Nonextremal-Pair-Rect N F)
                                                   -> (Listof (Nonextremal-Pair-Rect N F))))))
(define (make-pair-rect-subtract ops)
  (define empty? (set-sig-empty? ops))
  (define subtract (set-sig-subtract ops))
  (define intersect (set-sig-intersect ops))
  (λ (A B)
    (define A1 (Nonextremal-Pair-Rect-fst A))
    (define A2 (Nonextremal-Pair-Rect-snd A))
    (define B1 (Nonextremal-Pair-Rect-fst B))
    (define B2 (Nonextremal-Pair-Rect-snd B))
    (define C
      (let ([C1  (subtract A1 B1)])
        (if (empty? C1) empty-pair-set (Nonextremal-Pair-Rect C1 A2))))
    (define D
      (let ([D1  (intersect A1 B1)])
        (cond [(empty? D1)  empty-pair-set]
              [else
               (define D2 (subtract A2 B2))
               (if (empty? D2) empty-pair-set (Nonextremal-Pair-Rect D1 D2))])))
    (if (empty-pair-set? C)
        (if (empty-pair-set? D) '() (list D))
        (if (empty-pair-set? D) (list C) (list C D)))))

(: make-pair-rect-intersect
   (All (N F E V) ((set-sig N F E V)
                   -> ((Nonextremal-Pair-Rect N F) (Nonextremal-Pair-Rect N F)
                                                   -> (Listof (Nonextremal-Pair-Rect N F))))))
(define (make-pair-rect-intersect ops)
  (define empty? (set-sig-empty? ops))
  (define intersect (set-sig-intersect ops))
  (λ (A B)
    (define A1 (Nonextremal-Pair-Rect-fst A))
    (define A2 (Nonextremal-Pair-Rect-snd A))
    (define B1 (Nonextremal-Pair-Rect-fst B))
    (define B2 (Nonextremal-Pair-Rect-snd B))
    (define C1 (intersect A1 B1))
    (define C2 (intersect A2 B2))
    (cond [(or (empty? C1) (empty? C2))  '()]
          [else
           (define A1? (eq? C1 A1))
           (define A2? (eq? C2 A2))
           (cond [(and A1? A2?)  (list A)]
                 [else
                  (define B1? (eq? C1 B1))
                  (define B2? (eq? C2 B2))
                  (cond [(and B1? B2?)  (list B)]
                        [else
                         (list (Nonextremal-Pair-Rect (if A1? A1 (if B1? B1 C1))
                                                      (if A2? A2 (if B2? B2 C2))))])])])))

(: make-pair-rect-member?
   (All (N F E V) ((set-sig N F E V) -> ((Nonextremal-Pair-Rect N F) (Pair V V) -> Boolean))))
(define (make-pair-rect-member? ops)
  (define member? (set-sig-member? ops))
  (λ (A x)
    (and (member? (Nonextremal-Pair-Rect-fst A) (car x))
         (member? (Nonextremal-Pair-Rect-snd A) (cdr x)))))

;; ===================================================================================================
;; Complement and difference

(: make-rect-list-subtract1
   (All (N F E V) ((set-sig N F E V)
                   -> ((Listof (Nonextremal-Pair-Rect N F))
                       (Nonextremal-Pair-Rect N F)
                       -> (Listof (Nonextremal-Pair-Rect N F))))))
(define (make-rect-list-subtract1 ops)
  (define pair-rect-subtract ((inst make-pair-rect-subtract N F E V) ops))
  (λ (As B)
    (let loop ([As As] [B B])
      (cond [(empty? As)  empty]
            [else  (append (pair-rect-subtract (first As) B)
                           (loop (rest As) B))]))))

(: make-nonextremal-pair-set-complement
   (All (N F E V) ((set-sig N F E V) -> ((Nonextremal-Pair-Set N F) -> (Nonfull-Pair-Set N F)))))
(define (make-nonextremal-pair-set-complement ops)
  (define pair-rect-complement ((inst make-pair-rect-complement N F E V) ops))
  (define rect-list-subtract1 ((inst make-rect-list-subtract1 N F E V) ops))
  (λ (B)
    (define Bs (nonfull-pair-set->list B))
    (define As (pair-rect-complement (first Bs)))
    (list->pair-set
     (for/fold: ([As : (Listof (Nonextremal-Pair-Rect N F))  As]) ([B  (in-list (rest Bs))])
       (rect-list-subtract1 As B)))))

(: make-pair-set-complement
   (All (N F E V) ((set-sig N F E V)
                   -> (case->
                       ((Nonextremal-Pair-Set N F) -> (Nonextremal-Pair-Set N F))
                       ((Nonfull-Pair-Set N F) -> (Nonempty-Pair-Set N F))
                       ((Nonempty-Pair-Set N F) -> (Nonfull-Pair-Set N F))
                       ((Pair-Set N F) -> (Pair-Set N F))))))
(define (make-pair-set-complement ops)
  (define nonextremal-pair-set-complement ((inst make-nonextremal-pair-set-complement N F E V) ops))
  (λ (B)
    (cond [(empty-pair-set? B)  pairs]
          [(pairs? B)   empty-pair-set]
          [else
           (define C (nonextremal-pair-set-complement B))
           (cond [(empty-pair-set? C)
                  (raise-argument-error 'pair-set-complement "nonfull Nonextremal-Pair-Set" B)]
                 [else  C])])))

(: make-pair-set-subtract
   (All (N F E V) ((set-sig N F E V)
                   -> (case->
                       (Full-Pair-Set (Nonextremal-Pair-Set N F) -> (Nonextremal-Pair-Set N F))
                       (Full-Pair-Set (Nonfull-Pair-Set N F) -> (Nonempty-Pair-Set N F))
                       ((Pair-Set N F) (Nonempty-Pair-Set N F) -> (Nonfull-Pair-Set N F))
                       ((Pair-Set N F) (Pair-Set N F) -> (Pair-Set N F))))))
(define (make-pair-set-subtract ops)
  (define pair-set-complement ((inst make-pair-set-complement N F E V) ops))
  (define rect-list-subtract1 ((inst make-rect-list-subtract1 N F E V) ops))
  (λ (A B)
    (cond [(pairs? A)  (pair-set-complement B)]
          [(empty-pair-set? B)  A]
          [(empty-pair-set? A)  A]
          [(pairs? B)  empty-pair-set]
          [else
           (define As (nonfull-pair-set->list A))
           (define Bs (nonfull-pair-set->list B))
           (list->pair-set
            (for/fold: ([As : (Listof (Nonextremal-Pair-Rect N F))  As]) ([B  (in-list Bs)])
              (rect-list-subtract1 As B)))])))

;; ===================================================================================================
;; Intersection

(: make-rect-list-intersect1
   (All (N F E V) ((set-sig N F E V)
                   -> ((Listof (Nonextremal-Pair-Rect N F))
                       (Nonextremal-Pair-Rect N F)
                       -> (Listof (Nonextremal-Pair-Rect N F))))))
(define (make-rect-list-intersect1 ops)
  (define pair-rect-intersect ((inst make-pair-rect-intersect N F E V) ops))
  (λ (As B)
    (let loop ([As As] [B B])
      (cond [(empty? As)  empty]
            [else  (append (pair-rect-intersect (first As) B)
                           (loop (rest As) B))]))))

(: make-pair-set-intersect
   (All (N F E V) ((set-sig N F E V)
                   -> (case-> ((Pair-Set N F) (Nonfull-Pair-Set N F) -> (Nonfull-Pair-Set N F))
                              ((Nonfull-Pair-Set N F) (Pair-Set N F) -> (Nonfull-Pair-Set N F))
                              ((Pair-Set N F) (Pair-Set N F) -> (Pair-Set N F))))))
(define (make-pair-set-intersect ops)
  (define rect-list-intersect1 ((inst make-rect-list-intersect1 N F E V) ops))
  (λ (A B)
    (cond [(empty-pair-set? A)  A]
          [(empty-pair-set? B)  B]
          [(pairs? A)  B]
          [(pairs? B)  A]
          [else
           (define As (nonfull-pair-set->list A))
           (define Bs (nonfull-pair-set->list B))
           (list->pair-set
            (append*
             (for/list: : (Listof (Listof (Nonextremal-Pair-Rect N F))) ([B  (in-list Bs)])
               (rect-list-intersect1 As B))))])))

;; ===================================================================================================
;; Union

(: make-pair-set-union
   (All (N F E V) ((set-sig N F E V)
                   -> (case-> ((Pair-Set N F) (Nonempty-Pair-Set N F) -> (Nonempty-Pair-Set N F))
                              ((Nonempty-Pair-Set N F) (Pair-Set N F) -> (Nonempty-Pair-Set N F))
                              ((Pair-Set N F) (Pair-Set N F) -> (Pair-Set N F))))))
(define (make-pair-set-union ops)
  (define pair-set-subtract ((inst make-pair-set-subtract N F E V) ops))
  (define pair-set-complement ((inst make-pair-set-complement N F E V) ops))
  (define nonextremal-pair-set-complement ((inst make-nonextremal-pair-set-complement N F E V) ops))
  (λ (A B)
    (cond [(empty-pair-set? A)  B]
          [(empty-pair-set? B)  A]
          [(pairs? A)  A]
          [(pairs? B)  B]
          [else  (pair-set-complement (pair-set-subtract (pair-set-complement A) B))]
          #;; May return an apparently nonextremal set that is actually full
          [else
           (list->pair-set
            ;; Asserting because `append' doesn't preserve nonemptiness
            (assert (append (nonfull-pair-set->list (pair-set-subtract A B))
                            (nonfull-pair-set->list B))
                    pair?))])))

;; ===================================================================================================
;; Predicates

(: make-pair-set-subseteq?
   (All (N F E V) ((set-sig N F E V) -> ((Pair-Set N F) (Pair-Set N F) -> Boolean))))
(define (make-pair-set-subseteq? ops)
  (define pair-set-subtract ((inst make-pair-set-subtract N F E V) ops))
  (λ (A B)
    (empty-pair-set? (pair-set-subtract A B))))

(: make-pair-set-member?
   (All (N F E V) ((set-sig N F E V) -> ((Pair-Set N F) (Pair V V) -> Boolean))))
(define (make-pair-set-member? ops)
  (define pair-rect-member? ((inst make-pair-rect-member? N F E V) ops))
  (λ (A x)
    (cond [(empty-pair-set? A)  #f]
          [(pairs? A)   #t]
          [else
           (ormap (λ: ([A : (Nonextremal-Pair-Rect N F)])
                    (pair-rect-member? A x))
                  (nonfull-pair-set->list A))])))

;; ===================================================================================================
;; Mapping

(: make-pair-set-map
   (All (N F E V) ((set-sig N F E V)
                   -> (All (S) (S (S S -> S) -> (((U N F) (U N F) -> S) (Pair-Set N F) -> S))))))
(define (make-pair-set-map ops)
  (define full ((inst set-sig-full N F E V) ops))
  (λ (empty union)
    (λ (f A)
      (cond [(empty-pair-set? A)  empty]
            [(pairs? A)   (f full full)]
            [else
             (define As (map (λ: ([A : (Nonextremal-Pair-Rect N F)])
                               (f (Nonextremal-Pair-Rect-fst A)
                                  (Nonextremal-Pair-Rect-snd A)))
                             (nonfull-pair-set->list A)))
             (foldr union empty As)]))))
