#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         math/flonum
         math/private/utils
         "types.rkt"
         "real-set.rkt"
         "bool-set.rkt"
         "null-set.rkt"
         "pair-set.rkt"
         "extremal-set.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(define-type Pair-Index (U 'fst 'snd Natural))

;; ===================================================================================================
;; Tags

(define-type Tag Symbol)

;; Basic set tags
(define real-tag 'real)
(define bool-tag 'bool)
(define null-tag 'null)
(define pair-tag 'pair)

;; Set tags
(: make-set-tag (Symbol -> Tag))
(define (make-set-tag sym)
  (string->uninterned-symbol (symbol->string sym)))

;; ===================================================================================================
;; Set types

(define-type Nonextremal-Set
  (Rec N (U (Nonextremal-Basic* N) Full-Basic  ; i.e. Bot-Basic
            Top-Basic
            Bot-Tagged Top-Tagged
            Bot-Union Top-Union)))

(define-type Nonempty-Set (U Nonextremal-Set Universe))
(define-type  Nonfull-Set (U Nonextremal-Set Empty-Set))
(define-type          Set (U Nonextremal-Set Universe Empty-Set))

;; ===================================================================================================
;; Basic sets

(define-type  Full-Basic (U  Full-Real-Set  Full-Bool-Set  Full-Null-Set  Full-Pair-Set))
(define-type Empty-Basic (U Empty-Real-Set Empty-Bool-Set Empty-Null-Set Empty-Pair-Set))

(define-type (Nonextremal-Basic* N)
  (U Nonextremal-Real-Set
     Nonextremal-Bool-Set
     ;Nonextremal-Null-Set  ; there aren't any nonextremal null sets
     (Nonextremal-Pair-Set N Universe)))

(define-type Nonextremal-Basic (Nonextremal-Basic* Nonextremal-Set))
(define-type Nonempty-Basic (U Nonextremal-Basic  Full-Basic))
(define-type  Nonfull-Basic (U Nonextremal-Basic Empty-Basic))
(define-type          Basic (U Nonextremal-Basic  Full-Basic Empty-Basic))

(define-match-expander pair-rect
  (syntax-rules ()
    [(_ A1 A2)  (and (? Nonextremal-Pair-Rect?)
                     (app (inst Nonextremal-Pair-Rect-fst Nonextremal-Set Universe) A1)
                     (app (inst Nonextremal-Pair-Rect-snd Nonextremal-Set Universe) A2))])
  (make-head-form #'(inst Nonextremal-Pair-Rect Nonextremal-Set Universe)))

(define-syntax pair-rect?
  (make-rename-transformer #'Nonextremal-Pair-Rect?))

(define-syntax pair-rect-list
  (make-head-form #'(inst Nonextremal-Pair-Rect-List Nonextremal-Set Universe)))

(define-syntax pair-rect-list?
  (make-rename-transformer #'Nonextremal-Pair-Rect-List?))

(define-syntax pair-rect-list-elements
  (make-head-form #'(inst Nonextremal-Pair-Rect-List-elements Nonextremal-Set Universe)))

;; ===================================================================================================
;; Top and bottom basic sets

(define-type Bot-Basic Nonempty-Basic)

#|
(define-syntax bot-basic-set (make-rename-transformer #'Bot-Basic-set))
(define-syntax bot-basic? (make-rename-transformer #'Bot-Basic?))
|#

(define-syntax top-basic-set (make-rename-transformer #'Top-Basic-set))
(define-syntax top-basic? (make-rename-transformer #'Top-Basic?))

#|
(: print-bot-basic (Bot-Basic Output-Port (U #t #f 0 1) -> Any))
(define (print-bot-basic A port mode)
  (let ([A  (bot-basic-set A)])
    (cond
      ;; Nonempty-Real-Set
      [(full-real-set? A)  (write-string "reals" port)]
      [(Nonextremal-Interval? A)
       (match-define (Nonextremal-Interval a b a? b?) A)
       (cond [(and a? b?)  (pretty-print-constructor 'real-set (list a b) port mode)]
             [else  (pretty-print-constructor 'real-set (list a b a? b?) port mode)])]
      [(interval-list? A)
       (define As (map Bot-Basic (interval-list-elements A)))
       (pretty-print-constructor 'set-union As port mode)]
      ;; Nonempty-Bool-Set
      [(full-bool-set? A)   (write-string "bools" port)]
      [(true-bool-set? A)   (write-string "trues" port)]
      [(false-bool-set? A)  (write-string "falses" port)]
      ;; Full-Null-Set
      [(full-null-set? A)  (write-string "nulls" port)]
      ;; Nonempty-Pair-Set
      [(full-pair-set? A)  (write-string "pairs" port)]
      [(pair-rect? A)
       (match-define (pair-rect A1 A2) A)
       (pretty-print-constructor 'set-pair (list A1 A2) port mode)]
      [(pair-rect-list? A)
       (define As (map Bot-Basic (pair-rect-list-elements A)))
       (pretty-print-constructor 'set-union As port mode)]
      ;; Shouldn't happen
      [else
       (pretty-print-constructor 'bot-basic (list A) port mode)])))
|#

(: print-top-basic (Top-Basic Output-Port (U #t #f 0 1) -> Any))
(define (print-top-basic A port mode)
  (let ([A  (top-basic-set A)])
    (cond [(empty-real-set? A)  (write-string "not-reals" port)]
          [(empty-bool-set? A)  (write-string "not-bools" port)]
          [(empty-null-set? A)  (write-string "not-nulls" port)]
          [(empty-pair-set? A)  (write-string "not-pairs" port)]
          [else  (pretty-print-constructor 'top-basic (list A) port mode)])))

(struct: Top-Basic Base-Top-Entry ([set : Nonfull-Basic])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-top-basic)

(: basic-tag (Basic -> Tag))
(define (basic-tag A)
  (cond [(real-set? A)  real-tag]
        [(bool-set? A)  bool-tag]
        [(null-set? A)  null-tag]
        [else           pair-tag]))

(: top-basic-tag (Top-Basic -> Tag))
(define (top-basic-tag A) (basic-tag (top-basic-set A)))

(: bot-basic (case-> (Bot-Basic -> Bot-Basic)
                     (Basic -> (U Bot-Basic Empty-Set))))
(define (bot-basic A)
  (if (or (empty-real-set? A) (empty-bool-set? A) (empty-null-set? A) (empty-pair-set? A))
      empty-set
      A))

(: top-basic (case-> (Nonfull-Basic -> Top-Basic)
                     (Basic -> (U Top-Basic Universe))))
(define (top-basic A)
  (if (or (reals? A) (bools? A) (nulls? A) (pairs? A))
      universe
      (Top-Basic A)))

(define not-reals (top-basic empty-real-set))
(define not-bools (top-basic empty-bool-set))
(define not-nulls (top-basic empty-null-set))
(define not-pairs (top-basic empty-pair-set))

;; ===================================================================================================
;; Tagged sets

(define-syntax bot-tagged-tag (make-rename-transformer #'Bot-Tagged-tag))
(define-syntax bot-tagged-set (make-rename-transformer #'Bot-Tagged-set))
(define-syntax bot-tagged? (make-rename-transformer #'Bot-Tagged?))

(define-syntax top-tagged-tag (make-rename-transformer #'Top-Tagged-tag))
(define-syntax top-tagged-set (make-rename-transformer #'Top-Tagged-set))
(define-syntax top-tagged? (make-rename-transformer #'Top-Tagged?))

(: print-bot-tagged (Bot-Tagged Output-Port (U #t #f 0 1) -> Any))
(define (print-bot-tagged A port mode)
  (match-define (Bot-Tagged tag val) A)
  (pretty-print-constructor 'bot-tagged (list tag val) port mode))

(: print-top-tagged (Top-Tagged Output-Port (U #t #f 0 1) -> Any))
(define (print-top-tagged A port mode)
  (match-define (Top-Tagged tag val) A)
  (pretty-print-constructor 'top-tagged (list tag val) port mode))

(struct: Bot-Tagged Base-Bot-Entry ([tag : Tag] [set : Nonempty-Set])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-bot-tagged)

(struct: Top-Tagged Base-Top-Entry ([tag : Tag] [set : Nonfull-Set])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-top-tagged)

(: bot-tagged (case-> (Tag Nonempty-Set -> Bot-Tagged)
                      (Tag Set -> (U Bot-Tagged Empty-Set))))
(define (bot-tagged tag A)
  (if (empty-set? A) A (Bot-Tagged tag A)))

(: top-tagged (case-> (Tag Nonfull-Set -> Top-Tagged)
                      (Tag Set -> (U Top-Tagged Universe))))
(define (top-tagged tag A)
  (if (universe? A) A (Top-Tagged tag A)))

;; ===================================================================================================
;; Unions

(define-type Bot-Entry (U Bot-Basic Bot-Tagged))
(define-type Top-Entry (U Top-Basic Top-Tagged))

(: bot-tag (Bot-Entry -> Tag))
(define (bot-tag A)
  (if (bot-basic? A) (basic-tag A) (bot-tagged-tag A)))

(: top-tag (Top-Entry -> Tag))
(define (top-tag A)
  (if (top-basic? A) (top-basic-tag A) (top-tagged-tag A)))

(define-type Bot-Union-Hash (HashTable Tag Bot-Entry))
(define-type Top-Union-Hash (HashTable Tag Top-Entry))

(: print-bot-union (Bot-Union Output-Port (U #t #f 0 1) -> Any))
(define (print-bot-union A port mode)
  (pretty-print-constructor 'bot-union (hash-values (Bot-Union-hash A)) port mode))

(: print-top-union (Top-Union Output-Port (U #t #f 0 1) -> Any))
(define (print-top-union A port mode)
  (pretty-print-constructor 'top-union (hash-values (Top-Union-hash A)) port mode))

(struct: Bot-Union Base-Bot-Set ([hash : Bot-Union-Hash])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-bot-union)

(struct: Top-Union Base-Top-Set ([hash : Top-Union-Hash])
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
  (cond [(empty? As)  (Top-Union ((inst hasheq2 Tag Top-Entry) (top-tag A0) A0 (top-tag A1) A1))]
        [else  (Top-Union (make-immutable-hash
                           (map (λ: ([A : Top-Entry]) (cons (top-tag A) A))
                                (list* A0 A1 As))))]))

(: bot-union-sets ((U Empty-Set Bot-Entry Bot-Union) -> (Listof Bot-Entry)))
(define (bot-union-sets A)
  (cond [(empty-set? A)  empty]
        [(or (bot-basic? A) (bot-tagged? A))  (list A)]
        [else  (hash-values (Bot-Union-hash A))]))

(: top-union-sets ((U Universe Top-Entry Top-Union) -> (Listof Top-Entry)))
(define (top-union-sets A)
  (cond [(universe? A)  empty]
        [(or (top-basic? A) (top-tagged? A))  (list A)]
        [else  (hash-values (Top-Union-hash A))]))

(: bot-union-ref ((U Empty-Set Bot-Entry Bot-Union) Tag -> (U Empty-Set Bot-Entry)))
(define (bot-union-ref A tag)
  (cond [(empty-set? A)  A]
        [(bot-entry? A)  (if (eq? tag (bot-tag A)) A empty-set)]
        [else  (hash-ref (Bot-Union-hash A) tag (λ () empty-set))]))

(: top-union-ref ((U Universe Top-Entry Top-Union) Tag -> (U Universe Top-Entry)))
(define (top-union-ref A tag)
  (cond [(universe? A)   A]
        [(top-entry? A)  (if (eq? tag (top-tag A)) A universe)]
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
         (define a-tag (top-tag A))
         (define c-tag (top-tag C))
         (cond [(and (top-entry? C) (eq? c-tag a-tag))  C]
               [else  (Top-Union (hasheq2 a-tag A c-tag C))])]
        [else
         (Top-Union (hash-set (Top-Union-hash A) (top-tag C) C))]))

(: top-union-remove ((U Universe Top-Entry Top-Union) Tag -> (U Universe Top-Entry Top-Union)))
(define (top-union-remove A tag)
  (cond [(universe? A)   A]
        [(top-entry? A)  (if (eq? (top-tag A) tag) universe A)]
        [else  (define h (hash-remove (Top-Union-hash A) tag))
               (define n (hash-count h))
               (cond [(= n 0)  universe]
                     [(= n 1)  (first (hash-values h))]
                     [else  (Top-Union h)])]))
