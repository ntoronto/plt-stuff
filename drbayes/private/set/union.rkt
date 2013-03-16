#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         math/flonum
         math/private/utils
         "extremal-set.rkt"
         "interval.rkt"
         "boolean-rect.rkt"
         "null-rect.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Tagged sets and values

(struct: (T S) Tagged ([tag : T] [val : S]) #:transparent)

(define-syntax tagged (make-rename-transformer #'Tagged))
(define-syntax tagged? (make-rename-transformer #'Tagged?))
(define-syntax get-tag (make-rename-transformer #'Tagged-tag))
(define-syntax get-val (make-rename-transformer #'Tagged-val))

;; ===================================================================================================
;; Tags

;; Rectangle tags
(define-type Rect-Tag (U 'real 'null 'pair 'boolean))

(define real-tag 'real)
(define null-tag 'null)
(define pair-tag 'pair)
(define boolean-tag 'boolean)

;; Set tags
(define-type Set-Tag Symbol)

(: make-set-tag (Symbol -> Set-Tag))
(define (make-set-tag sym) (string->uninterned-symbol (symbol->string sym)))

;; ===================================================================================================
;; Set types

(define-type Rect (U Interval Null-Rect Pair-Rect Boolean-Rect))
(define-type Maybe-Rect (U Empty-Set Rect))

(define-type Top-Rect (Top-Rect-Struct Rect-Tag Maybe-Rect))

(define-type Tagged-Set (U Bot-Set Top-Set))
(define-type Bot-Set (Bot-Set-Struct Set-Tag Nonempty-Set))
(define-type Top-Set (Top-Set-Struct Set-Tag Nonfull-Set))

(define-type Nonextremal-Set
  (Rec Nonextremal-Set
       (U Rect Top-Rect Bot-Union Top-Union
          ;; The union of these next two are equivalent to Tagged-Set
          (Bot-Set-Struct Set-Tag (U Nonextremal-Set Universe))
          (Top-Set-Struct Set-Tag (U Nonextremal-Set Empty-Set)))))

(define-type Nonempty-Set (U Nonextremal-Set Universe))
(define-type Nonfull-Set (U Nonextremal-Set Empty-Set))
(define-type Set (U Nonextremal-Set Empty-Set Universe))

;; ===================================================================================================
;; Pair rectangles

(define-type Pair-Index (U 'fst 'snd Natural))

(: print-pair-rect (Pair-Rect Output-Port (U #t #f 0 1) -> Any))
(define (print-pair-rect p port mode)
  (pretty-print-constructor 'pair-rect (list (Pair-Rect-fst p) (Pair-Rect-snd p)) port mode))

(struct: Pair-Rect ([fst : Nonempty-Set] [snd : Nonempty-Set])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-pair-rect)

(define-type Maybe-Pair-Rect (U Empty-Set Pair-Rect))

(define-syntax pair-rect (make-rename-transformer #'Pair-Rect))
(define-syntax pair-rect? (make-rename-transformer #'Pair-Rect?))
(define-syntax pair-rect-fst (make-rename-transformer #'Pair-Rect-fst))
(define-syntax pair-rect-snd (make-rename-transformer #'Pair-Rect-snd))

;; ===================================================================================================
;; Top rectangles

(: rect? (Set -> Boolean : Rect))
(define rect? (λ: ([A : Set]) (or (interval? A) (null-rect? A) (pair-rect? A) (boolean-rect? A))))

(: rect-tag (Rect -> Rect-Tag))
(define (rect-tag A)
  (cond [(interval? A)   real-tag]
        [(null-rect? A)  null-tag]
        [(pair-rect? A)  pair-tag]
        [(boolean-rect? A)  boolean-tag]))

(: print-top-rect (Top-Rect Output-Port (U #t #f 0 1) -> Any))
(define (print-top-rect A port mode)
  (match-define (Tagged tag val) A)
  (cond [(and (eq? tag real-tag) (empty-set? val))     (write-string "not-reals" port)]
        [(and (eq? tag null-tag) (empty-set? val))     (write-string "not-null-set" port)]
        [(and (eq? tag pair-tag) (empty-set? val))     (write-string "not-pairs" port)]
        [(and (eq? tag boolean-tag) (empty-set? val))  (write-string "not-booleans" port)]
        [else
         (pretty-print-constructor 'top-rect (list tag val) port mode)]))

(struct: (T S) Top-Rect-Struct Tagged ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-top-rect)

(define-syntax top-rect? (make-rename-transformer #'Top-Rect-Struct?))

(: top-rect (Rect-Tag Maybe-Rect -> Top-Rect))
(define (top-rect tag A)
  (cond [(empty-set? A)  (Top-Rect-Struct tag empty-set)]
        [(eq? tag (rect-tag A))  (Top-Rect-Struct tag A)]
        [else  (raise-argument-error 'top-rect (format "~v" (rect-tag A)) 0 tag A)]))

;; ---------------------------------------------------------------------------------------------------
;; Tagged intervals

(define all-reals real-interval)
(define not-reals (top-rect real-tag empty-set))

;; ---------------------------------------------------------------------------------------------------
;; Tagged null rect

(define null-set null-rect)
(define not-null-set (top-rect null-tag empty-set))

;; ---------------------------------------------------------------------------------------------------
;; Tagged pair rects

(define all-pairs (pair-rect universe universe))
(define not-pairs (top-rect pair-tag empty-set))

(: set-pair (case-> (Nonempty-Set Nonempty-Set -> Pair-Rect)
                    (Empty-Set Set -> Empty-Set)
                    (Set Empty-Set -> Empty-Set)
                    (Set Set -> (U Empty-Set Pair-Rect))))
(define (set-pair A1 A2)
  (cond [(empty-set? A1)  A1]
        [(empty-set? A2)  A2]
        [(and (universe? A1) (universe? A2))  all-pairs]
        [else  (pair-rect A1 A2)]))

(: set-list (case-> (Nonempty-Set * -> Rect)
                    (Set * -> (U Empty-Set Rect))))
(define (set-list . As)
  (foldr set-pair null-set As))

(: set-list* (case-> (Nonempty-Set Nonempty-Set * -> Nonempty-Set)
                     (Set Set * -> Set)))
(define (set-list* A . As)
  (let loop ([A A] [As As])
    (cond [(empty? As)  A]
          [else  (set-pair A (loop (first As) (rest As)))])))

;; ---------------------------------------------------------------------------------------------------
;; Tagged boolean rects

(define not-booleans (top-rect boolean-tag empty-set))

(: boolean-rect (case-> (-> Empty-Set)
                        (Boolean -> Rect)
                        (Boolean Boolean -> Rect)))
(define boolean-rect
  (case-lambda
    [()  empty-set]
    [(b0)  (if b0 trues falses)]
    [(b0 b1)  (if b0 (if b1 trues booleans) (if b1 booleans falses))]))

;; ===================================================================================================
;; Tagged sets

(: print-bot-set (Bot-Set Output-Port (U #t #f 0 1) -> Any))
(define (print-bot-set A port mode)
  (match-define (Tagged tag val) A)
  (pretty-print-constructor 'bot-set (list tag val) port mode))

(: print-top-set (Top-Set Output-Port (U #t #f 0 1) -> Any))
(define (print-top-set A port mode)
  (match-define (Tagged tag val) A)
  (pretty-print-constructor 'top-set (list tag val) port mode))

(struct: (T S) Bot-Set-Struct Tagged ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-bot-set)

(struct: (T S) Top-Set-Struct Tagged ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-top-set)

(define-syntax bot-set? (make-rename-transformer #'Bot-Set-Struct?))
(define-syntax top-set? (make-rename-transformer #'Top-Set-Struct?))

(: bot-set (case-> (Set-Tag Nonempty-Set -> Bot-Set)
                   (Set-Tag Empty-Set -> Empty-Set)
                   (Set-Tag Set -> (U Empty-Set Bot-Set))))
(define (bot-set tag A)
  (if (empty-set? A) A (Bot-Set-Struct tag A)))

(: top-set (case-> (Set-Tag Nonfull-Set -> Top-Set)
                   (Set-Tag Universe -> Universe)
                   (Set-Tag Set -> (U Universe Top-Set))))
(define (top-set tag A)
  (if (universe? A) A (Top-Set-Struct tag A)))

;; ===================================================================================================
;; Unions

(define-type Tag (U Rect-Tag Set-Tag))

(define-type Bot-Entry (U Rect Bot-Set))
(define-type Top-Entry (U Top-Rect Top-Set))

(define bot-entry? (λ: ([A : Set]) (or (rect? A) (bot-set? A))))
(define top-entry? (λ: ([A : Set]) (or (top-rect? A) (top-set? A))))

(: bot-tag ((U Rect Bot-Set) -> Symbol))
(define (bot-tag A)
  (cond [(bot-set? A)  (get-tag A)]
        [(interval? A)  real-tag]
        [(null-rect? A)  null-tag]
        [(pair-rect? A)  pair-tag]
        [(boolean-rect? A)  boolean-tag]))


(define-type Bot-Union-Hash (HashTable Tag Bot-Entry))
(define-type Top-Union-Hash (HashTable Tag Top-Entry))

(: print-bot-union (Bot-Union Output-Port (U #t #f 0 1) -> Any))
(define (print-bot-union A port mode)
  (pretty-print-constructor 'bot-union (hash-values (Bot-Union-hash A)) port mode))

(: print-top-union (Top-Union Output-Port (U #t #f 0 1) -> Any))
(define (print-top-union A port mode)
  (pretty-print-constructor 'top-union (hash-values (Top-Union-hash A)) port mode))

(struct: Bot-Union ([hash : Bot-Union-Hash])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-bot-union)

(struct: Top-Union ([hash : Top-Union-Hash])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-top-union)

(define-syntax bot-union? (make-rename-transformer #'Bot-Union?))
(define-syntax top-union? (make-rename-transformer #'Top-Union?))

(: bot-union (Bot-Entry Bot-Entry Bot-Entry * -> Bot-Union))
(define (bot-union A0 A1 . As)
  (cond [(empty? As)  (Bot-Union ((inst hasheq2 Tag Bot-Entry) (bot-tag A0) A0 (bot-tag A1) A1))]
        [else  (Bot-Union (make-immutable-hash
                           (map (λ: ([A : Bot-Entry]) (cons (bot-tag A) A))
                                (list* A0 A1 As))))]))

(: top-union (Top-Entry Top-Entry Top-Entry * -> Top-Union))
(define (top-union A0 A1 . As)
  (cond [(empty? As)  (Top-Union ((inst hasheq2 Tag Top-Entry) (get-tag A0) A0 (get-tag A1) A1))]
        [else  (Top-Union (make-immutable-hash
                           (map (λ: ([A : Top-Entry]) (cons (get-tag A) A))
                                (list* A0 A1 As))))]))

(: bot-union-sets ((U Empty-Set Bot-Entry Bot-Union) -> (Listof Bot-Entry)))
(define (bot-union-sets A)
  (cond [(empty-set? A)  empty]
        [(or (rect? A) (bot-set? A))  (list A)]
        [else  (hash-values (Bot-Union-hash A))]))

(: top-union-sets ((U Universe Top-Entry Top-Union) -> (Listof Top-Entry)))
(define (top-union-sets A)
  (cond [(universe? A)  empty]
        [(or (top-rect? A) (top-set? A))  (list A)]
        [else  (hash-values (Top-Union-hash A))]))

(: bot-union-ref ((U Empty-Set Bot-Entry Bot-Union) Tag -> (U Empty-Set Bot-Entry)))
(define (bot-union-ref A tag)
  (cond [(empty-set? A)  A]
        [(bot-entry? A)  (if (eq? tag (bot-tag A)) A empty-set)]
        [else  (hash-ref (Bot-Union-hash A) tag (λ () empty-set))]))

(: top-union-ref ((U Universe Top-Entry Top-Union) Tag -> (U Universe Top-Entry)))
(define (top-union-ref A tag)
  (cond [(universe? A)   A]
        [(top-entry? A)  (if (eq? tag (get-tag A)) A universe)]
        [else  (hash-ref (Top-Union-hash A) tag (λ () universe))]))

(: bot-union-add ((U Empty-Set Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                                                    -> (U Bot-Entry Bot-Union)))
(define (bot-union-add A C)
  (cond [(empty-set? A)  C]
        [(bot-union? C)  (for/fold ([A A]) ([C  (in-list (bot-union-sets C))])
                           (bot-union-add A C))]
        [(bot-entry? A)
         (define a-tag (bot-tag A))
         (define c-tag (bot-tag C))
         (cond [(and (bot-entry? C) (eq? c-tag a-tag))  C]
               [else  (Bot-Union (hasheq2 a-tag A c-tag C))])]
        [else
         (Bot-Union (hash-set (Bot-Union-hash A) (bot-tag C) C))]))

(: bot-union-remove ((U Empty-Set Bot-Entry Bot-Union) Tag -> (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-union-remove A tag)
  (cond [(empty-set? A)  A]
        [(bot-entry? A)  (if (eq? (bot-tag A) tag) empty-set A)]
        [else  (define h (hash-remove (Bot-Union-hash A) tag))
               (define n (hash-count h))
               (cond [(= n 0)  empty-set]
                     [(= n 1)  (first (hash-values h))]
                     [else  (Bot-Union h)])]))

(: top-union-add ((U Universe Top-Entry Top-Union) (U Top-Entry Top-Union)
                                                   -> (U Top-Entry Top-Union)))
(define (top-union-add A C)
  (cond [(universe? A)  C]
        [(top-union? C)  (for/fold ([A A]) ([C  (in-list (top-union-sets C))])
                           (top-union-add A C))]
        [(top-entry? A)
         (define a-tag (get-tag A))
         (define c-tag (get-tag C))
         (cond [(and (top-entry? C) (eq? c-tag a-tag))  C]
               [else  (Top-Union (hasheq2 a-tag A c-tag C))])]
        [else
         (Top-Union (hash-set (Top-Union-hash A) (get-tag C) C))]))

(: top-union-remove ((U Universe Top-Entry Top-Union) Tag -> (U Universe Top-Entry Top-Union)))
(define (top-union-remove A tag)
  (cond [(universe? A)   A]
        [(top-entry? A)  (if (eq? (get-tag A) tag) universe A)]
        [else  (define h (hash-remove (Top-Union-hash A) tag))
               (define n (hash-count h))
               (cond [(= n 0)  universe]
                     [(= n 1)  (first (hash-values h))]
                     [else  (Top-Union h)])]))

;; ===================================================================================================
;; Set operations

(define set-nonempty-bot? (λ: ([A : Nonempty-Set]) (or (bot-union? A) (bot-set? A) (rect? A))))
(define set-bot? (λ: ([A : Set]) (or (empty-set? A) (set-nonempty-bot? A))))

(define set-nonfull-top? (λ: ([A : Nonfull-Set]) (or (top-union? A) (top-set? A) (top-rect? A))))
(define set-top? (λ: ([A : Set]) (or (universe? A) (set-nonfull-top? A))))

;; ---------------------------------------------------------------------------------------------------
;; Tagging and untagging

(: set-tag (case-> (Nonempty-Set Set-Tag -> Bot-Set)
                   (Empty-Set Set-Tag -> Empty-Set)
                   (Set Set-Tag -> (U Empty-Set Bot-Set))))
(define (set-tag A tag)
  (bot-set tag A))

(: set-untag (case-> (Empty-Set Set-Tag -> Empty-Set)
                     (Universe Set-Tag  -> Universe)
                     (Set Set-Tag -> Set)))
(define (set-untag A tag)
  (cond [(empty-set? A)  empty-set]
        [(universe? A)   universe]
        [(rect? A)       empty-set]
        [(bot-set? A)    (if (eq? tag (get-tag A)) (get-val A) empty-set)]
        [(bot-union? A)  (set-untag (bot-union-ref A tag) tag)]
        [(top-rect? A)   universe]
        [(top-set? A)    (if (eq? tag (get-tag A)) (get-val A) universe)]
        [(top-union? A)  (set-untag (top-union-ref A tag) tag)]))

(: rect-untag (case-> (Empty-Set Rect-Tag -> Empty-Set)
                      (Set Rect-Tag -> Maybe-Rect)))
(define (rect-untag A tag)
  (cond [(empty-set? A)  empty-set]
        [(universe? A)   (cond [(eq? tag real-tag)  real-interval]
                               [(eq? tag null-tag)  null-rect]
                               [(eq? tag pair-tag)  all-pairs]
                               [(eq? tag boolean-tag)  booleans]
                               [else  empty-set])]
        [(interval? A)   (if (eq? tag real-tag) A empty-set)]
        [(null-rect? A)  (if (eq? tag null-tag) A empty-set)]
        [(pair-rect? A)  (if (eq? tag pair-tag) A empty-set)]
        [(boolean-rect? A)  (if (eq? tag boolean-tag) A empty-set)]
        [(bot-set? A)    empty-set]
        [(bot-union? A)  (rect-untag (bot-union-ref A tag) tag)]
        [(top-rect? A)   (if (eq? tag (get-tag A)) (get-val A) (rect-untag universe tag))]
        [(top-set? A)    (rect-untag universe tag)]
        [(top-union? A)  (rect-untag (top-union-ref A tag) tag)]))

;; ---------------------------------------------------------------------------------------------------
;; Pair ref

(: set-pair-ref (Set Pair-Index -> Set))
(define (set-pair-ref A j)
  (let ([A  (rect-untag A pair-tag)])
    (cond [(and A (pair-rect? A))  (pair-rect-ref A j)]
          [else  empty-set])))

(: pair-rect-ref (Rect Pair-Index -> Set))
(define (pair-rect-ref A j)
  (match-define (pair-rect A1 A2) A)
  (cond [(eq? j 'fst)  A1]
        [(eq? j 'snd)  A2]
        [(zero? j)  A1]
        [else  (set-pair-ref A2 (- j 1))]))

;; ---------------------------------------------------------------------------------------------------
;; Pair set

(: set-pair-set (Set Pair-Index Set -> Set))
(define (set-pair-set A j C)
  (cond
    [(empty-set? C)  empty-set]
    [else
     (define Asub (rect-untag A pair-tag))
     (cond [(and Asub (pair-rect? Asub))
            (let ([Asub  (pair-rect-set Asub j C)])
              (if Asub Asub A))]
           [else  empty-set])]))

(: pair-rect-set (Rect Pair-Index Nonempty-Set -> (U #f Set)))
(define (pair-rect-set A j C)
  (match-define (pair-rect A1 A2) A)
  (cond [(eq? j 'fst)  (if (eq? C A1) #f (set-pair C A2))]
        [(eq? j 'snd)  (if (eq? C A2) #f (set-pair A1 C))]
        [(zero? j)     (if (eq? C A1) #f (set-pair C A2))]
        [else  (let ([C  (set-pair-set A2 (- j 1) C)])
                 (if (eq? C A2) #f (set-pair A1 C)))]))
