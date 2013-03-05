#lang typed/racket/base

(require racket/match
         racket/list
         math/flonum
         "utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Statistics

(define: search-stats : (HashTable Symbol Natural)  (make-hasheq empty))

(: increment-search-stat (Symbol -> Void))
(define (increment-search-stat name)
  (hash-set! search-stats name (+ 1 (hash-ref search-stats name (λ () 0)))))

(define (reset-search-stats)
  (set! search-stats ((inst make-hasheq Symbol Natural) empty)))

(define (get-search-stats)
  search-stats)

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
;; Search

(: sample-search-tree (All (T) ((Search-Tree T) Flonum -> (Values (Search-Tree T) Flonum))))
;; Straightforward sampler, useful only for rejection sampling
;; Returns a leaf in the search tree, and the probability of finding that leaf
(define (sample-search-tree t p)
  (cond [(search-node? t)
         (match-define (search-node cs qs name) t)
         (increment-search-stat name)
         (define i (sample-index qs))
         (define c (list-ref cs i))
         (define q (list-ref qs i))
         (sample-search-tree (maybe-force c) (* p q))]
        [else
         (values t p)]))

(: sample-search-tree/remove-failure
   (All (T) ((Search-Tree T) Flonum -> (Values (Search-Leaf T) Flonum (Search-Tree T) Flonum))))
;; Like `sample-search-tree', but when it returns failure, it also returns a tree without that failure
;; The returned tree is equivalent in the sense that successes are returned with the same
;; probabilities in future searches (up to floating-point error)
(define (sample-search-tree/remove-failure t p)
  (cond [(search-node? t)
         (match-define (search-node cs qs name) t)
         ;(increment-search-stat name)
         (define i (sample-index qs))
         (define c (list-ref cs i))
         (define q (list-ref qs i))
         (define pq (* p q))
         (let-values ([(s u c new-pq)  (sample-search-tree/remove-failure (maybe-force c) pq)])
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
;; Samples multiple success leaves using `sample-search-tree/remove-failure'
(define (sample-search-tree* t n)
  (cond
    [(index? n)
     (define: ss : (Listof (success-leaf T))  empty)
     (define: ps : (Listof Flonum)  empty)
     (let: loop ([i : Nonnegative-Fixnum  0] [ss ss] [ps ps] [t t] [q 1.0])
       (cond [(and (i . < . n) (q . > . 0.0) (not (failure-leaf? t)))
              (let-values ([(s p t q)  (sample-search-tree/remove-failure t q)])
                (cond [(success-leaf? s)
                       (let ([i  (+ i 1)])
                         (when (= 0 (remainder i 100))
                           (printf "i = ~v~n" i)
                           (flush-output))
                         (loop i (list* s ss) (cons p ps) t q))]
                      [else
                       (increment-search-stat 'restarts)
                       (loop i ss ps t q)]))]
             [else  (values ss ps)]))]
    [else
     (raise-argument-error 'sample-search-tree* "Index" 1 t n)]))

(define t (search-node (list (failure-leaf) (success-leaf 0 0.5)) (list 0.5 0.5) 'bob))
