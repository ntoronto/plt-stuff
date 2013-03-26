#lang typed/racket/base

(require racket/match
         racket/list
         math/flonum
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Statistics

(define search-stats? #t)

(define: search-stats : (HashTable Symbol Natural)  (make-hasheq empty))

(: increment-search-stat (Symbol -> Void))
(define (increment-search-stat name)
  (hash-set! search-stats name (+ 1 (hash-ref search-stats name (λ () 0)))))

(define (reset-search-stats)
  (set! search-stats ((inst make-hasheq Symbol Natural) empty)))

(: get-search-stats (-> (Listof (Pair Symbol Natural))))
(define (get-search-stats)
  ((inst sort (Pair Symbol Natural) String)
   (hash-map search-stats (λ: ([k : Symbol] [v : Natural]) (cons k v)))
   string<?
   #:key (λ: ([kv : (Pair Symbol Natural)]) (symbol->string (car kv)))
   #:cache-keys? #t))

;; ===================================================================================================
;; Search tree types

(struct: failure-leaf () #:transparent)
(struct: (T) success-leaf ([value : T] [prob : Flonum]) #:transparent)
(struct: (T) search-node ([trees : (Listof+2 (U (Promise (Search-Tree T)) (Search-Tree T)))]
                          [probs : (Listof+2 Flonum)]  ; assumes (flsum probs) ≈ 1.0
                          [name : Symbol])
  #:transparent)

(define-type (Search-Leaf T) (U failure-leaf (success-leaf T)))
(define-type (Search-Tree T) (U (Search-Leaf T) (search-node T)))

;; ===================================================================================================
;; Nonadaptive, independent search

(: sample-search-tree-once (All (T) ((Search-Tree T) Flonum -> (Values (Search-Tree T) Flonum))))
;; Straightforward sampler, useful only for rejection sampling
;; Returns a leaf in the search tree, and the probability of finding that leaf
(define (sample-search-tree-once t p)
  (cond [(search-node? t)
         (match-define (search-node cs qs name) t)
         (when search-stats? (increment-search-stat name))
         (define i (sample-index qs))
         (define c (list-ref cs i))
         (define q (list-ref qs i))
         (sample-search-tree-once (maybe-force c) (* p q))]
        [else
         (values t p)]))

(: sample-search-tree-once* (All (T) ((Search-Tree T) Integer -> (Values (Listof (success-leaf T))
                                                                         (Listof Flonum)))))
(define (sample-search-tree-once* t n)
  (cond
    [(not (index? n))   (raise-argument-error 'sample-search-tree-once* "Index" 1 t n)]
    [(failure-leaf? n)  (values empty empty)]
    [else
     (let: loop ([i : Nonnegative-Fixnum  0]
                 [ss : (Listof (success-leaf T))  empty]
                 [ps : (Listof Flonum)  empty])
       (cond [(i . < . n)
              (let ([i  (+ i 1)])
                (when (= 0 (remainder i 100))
                  (printf "i = ~v~n" i)
                  (flush-output))
                (let-values ([(s p)  (sample-search-tree-once t 1.0)])
                  (cond [(success-leaf? s)
                         (loop i (cons s ss) (cons p ps))]
                        [else
                         (when search-stats? (increment-search-stat 'restarts))
                         (loop i ss ps)])))]
             [else
              (values ss ps)]))]))

;; ===================================================================================================
;; Adaptive search

(: sample-search-tree
   (All (T) ((Search-Tree T) Flonum -> (Values (Search-Leaf T) Flonum (Search-Tree T) Flonum))))
;; Like `sample-search-tree', but when it returns failure, it also returns a tree without that failure
;; The returned tree is equivalent in the sense that successes are returned with the same
;; probabilities in future searches (up to floating-point error)
(define (sample-search-tree t p)
  (cond [(search-node? t)
         (match-define (search-node cs qs name) t)
         (when search-stats? (increment-search-stat name))
         (define i (sample-index qs))
         (define c (list-ref cs i))
         (define q (list-ref qs i))
         (define pq (* p q))
         (let-values ([(s u c new-pq)  (sample-search-tree (maybe-force c) pq)])
           (define new-t
             (cond [(new-pq . > . 0.0)
                    (let* ([cs  (list-set/+2 cs i c)]
                           [qs  (list-set/+2 qs i (/ new-pq p))]
                           [qs  (normalize-probs/+2 qs)])
                      (search-node cs qs name))]
                   [else
                    (let ([cs  (remove-index/+2 cs i)]
                          [qs  (remove-index/+2 qs i)])
                      (cond [(or (empty? (rest cs)) (empty? (rest qs)))
                             (maybe-force (first cs))]
                            [else
                             (search-node cs (normalize-probs/+2 qs) name)]))]))
           (values s u new-t (- p (- pq new-pq))))]
        [(failure-leaf? t)
         (values t p t 0.0)]
        [else
         (values t p t (success-leaf-prob t))]))

(: sample-search-tree* (All (T) ((Search-Tree T) Integer -> (Values (Listof (success-leaf T))
                                                                    (Listof Flonum)))))
;; Samples multiple success leaves using `sample-search-tree'
(define (sample-search-tree* t n)
  (cond
    [(index? n)
     (define: ss : (Listof (success-leaf T))  empty)
     (define: ps : (Listof Flonum)  empty)
     (let: loop ([i : Nonnegative-Fixnum  0] [ss ss] [ps ps] [t t] [q 1.0])
       (cond [(and (i . < . n) (q . > . 0.0) (not (failure-leaf? t)))
              (let-values ([(s p t q)  (sample-search-tree t q)])
                (cond [(success-leaf? s)
                       (let ([i  (+ i 1)])
                         (when (= 0 (remainder i 100))
                           (printf "i = ~v~n" i)
                           (flush-output))
                         (loop i (list* s ss) (cons p ps) t q))]
                      [else
                       (when search-stats? (increment-search-stat 'restarts))
                       (loop i ss ps t q)]))]
             [else  (values ss ps)]))]
    [else
     (raise-argument-error 'sample-search-tree* "Index" 1 t n)]))
