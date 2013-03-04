#lang typed/racket/base

(require racket/match
         racket/list
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
(struct: (T) success-leaf ([value : T]) #:transparent)
(struct: (T) search-node ([trees : (Listof+2 (U (Promise (Search-Tree T)) (Search-Tree T)))]
                          [probs : (Listof+2 Flonum)]  ; assumes (flsum probs) ≈ 1.0
                          [name : Symbol])
  #:transparent)

(define-type (Search-Leaf T) (U failure-leaf (success-leaf T)))
(define-type (Search-Tree T) (U (Search-Leaf T) (search-node T)))

;; ===================================================================================================
;; Search

(: sample-search-tree (All (T) ((Search-Tree T) -> (Values (Search-Tree T) Flonum))))
;; Straightforward sampler, useful only for rejection sampling
;; Returns a leaf in the search tree, and the probability of finding that leaf
(define (sample-search-tree t)
  (let loop ([t t] [p 1.0])
    (cond [(search-node? t)
           (match-define (search-node cs qs name) t)
           (increment-search-stat name)
           (define i (sample-index qs))
           (define c (list-ref cs i))
           (define q (list-ref qs i))
           (loop (maybe-force c) (* p q))]
          [else
           (values t p)])))

(: sample-search-tree/remove-failure
   (All (T) ((Search-Tree T) -> (Values (Search-Leaf T) Flonum (Search-Tree T)))))
;; Like `sample-search-tree', but when it returns failure, it also returns a tree without that failure
;; The returned tree is equivalent in the sense that successes are returned with the same
;; probabilities in future searches (up to floating-point error)
(define (sample-search-tree/remove-failure t)
  (cond [(search-node? t)
         (match-define (search-node cs qs name) t)
         ;(increment-search-stat name)
         (define i (sample-index qs))
         (define c (list-ref cs i))
         (define q (list-ref qs i))
         (let*-values ([(s p c)  (sample-search-tree/remove-failure (maybe-force c))]
                       [(p)  (* p q)])
           (define new-t
             (cond [(not (failure-leaf? s))  t]
                   [(q . > . p)
                    (let ([cs  (list-set/+2 cs i c)]
                          [qs  (normalize-probs/+2 (list-set/+2 qs i (- q p)))])
                      (search-node cs qs name))]
                   [else
                    (let ([cs  (remove-index/+2 cs i)]
                          [qs  (remove-index/+2 qs i)])
                      (cond [(or (empty? (rest cs)) (empty? (rest qs)))
                             (maybe-force (first cs))]
                            [else
                             (search-node cs (normalize-probs/+2 qs) name)]))]))
           (values s p new-t))]
        [else
         (values t 1.0 t)]))

(: sample-search-tree* (All (T) ((Search-Tree T) Integer -> (Values (Listof (success-leaf T))
                                                                    (Listof Flonum)))))
;; Samples multiple success leaves using `sample-search-tree/remove-failure'
(define (sample-search-tree* t n)
  (cond
    [(index? n)
     (define: ss : (Listof (success-leaf T))  empty)
     (define: ps : (Listof Flonum)  empty)
     (let: loop ([i : Nonnegative-Fixnum  0] [ss ss] [ps ps] [t t] [q 1.0])
       (cond [(i . < . n)
              (when (= 0 (remainder (+ i 1) 100))
                (printf "i = ~v~n" (+ i 1))
                (flush-output))
              (let*-values ([(s p t)  (sample-search-tree/remove-failure t)]
                            [(p)  (* p q)])
                (cond [(success-leaf? s)
                       (loop (+ i 1) (list* s ss) (cons p ps) t q)]
                      [else
                       (increment-search-stat 'backtracks)
                       (loop i ss ps t (- q p))]))]
             [else  (values ss ps)]))]
    [else
     (raise-argument-error 'sample-search-tree* "Index" 1 t n)]))
