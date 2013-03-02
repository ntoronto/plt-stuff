#lang typed/racket

(require "../private/omega.rkt")

;; ===================================================================================================
;; Randomized bisimulation tests for reffing and setting omega trees

(define-predicate zero-or-one? (U 0 1))

(define-type (Omega-Hash A) (HashTable Omega-Idx A))

(define: empty-omega-hash : (Omega-Hash Integer)  (make-immutable-hash empty))

(: omega-tree->omega-hash ((Omega-Tree Integer) -> (Omega-Hash Integer)))
(define (omega-tree->omega-hash t)
  (make-immutable-hash
   (let: loop : (Listof (Pair Omega-Idx Integer)) ([t : (Omega-Tree Integer)  t]
                                                   [idx : Omega-Idx  empty]
                                                   [kvs : (Listof (Pair Omega-Idx Integer))  empty])
     (cond [(omega-leaf? t)  kvs]
           [else
            (define v (Omega-Node-value t))
            (let ([kvs  (loop (Omega-Node-fst t) (cons 0 idx) kvs)])
              (cond [(equal? v 0)  (loop (Omega-Node-snd t) (cons 1 idx) kvs)]
                    [else  (loop (Omega-Node-snd t)
                                 (cons 1 idx)
                                 (cons (cons (reverse idx) v) kvs))]))]))))

(: omega-hash-ref ((Omega-Hash Integer) Omega-Idx -> Integer))
(define (omega-hash-ref h k)
  (hash-ref h k (λ () 0)))

(: omega-hash-set ((Omega-Hash Integer) Omega-Idx Integer -> (Omega-Hash Integer)))
(define (omega-hash-set h k i)
  (cond [(= i 0)  (hash-remove h k)]
        [else  (hash-set h k i)]))

(: random-omega-idx (-> Omega-Idx))
(define (random-omega-idx)
  (build-list (random 3) (λ (_) (assert (random 2) zero-or-one?))))

(: bisimulation-step ((Omega-Hash Integer) (Omega-Tree Integer) -> (Values (Omega-Hash Integer)
                                                                           (Omega-Tree Integer))))
(define (bisimulation-step h t)
  (define r (random))
  (cond [(r . < . 0.4)
         (define k (random-omega-idx))
         (define i (random 4))
         (define new-h (omega-hash-set h k i))
         (define new-t ((omega-tree-set 0) t k i))
         (unless (equal? new-h (omega-tree->omega-hash new-t))
           (error 'bisimulation-step "operation set ~v ~v failed with ~v and ~v" k i h t))
         (values new-h new-t)]
        [(r . < . 0.8)
         (define k (random-omega-idx))
         (define i0 (omega-hash-ref h k))
         (define i1 ((omega-tree-ref 0) t k))
         (unless (= i0 i1)
           (error 'bisimulation-step "operation ref ~v failed with ~v and ~v" k h t))
         (define new-h (omega-hash-set h k 0))
         (define new-t ((omega-tree-set 0) t k 0))
         (unless (equal? new-h (omega-tree->omega-hash new-t))
           (error 'bisimulation-step "operation set ~v 0 failed with ~v and ~v" k h t))
         (values new-h new-t)]
        [else
         (define k (random-omega-idx))
         (define i ((omega-tree-ref 0) t k))
         (define new-t ((omega-tree-set 0) t k i))
         (unless (eq? t new-t)
           (error 'bisimulation-step "eq? check failed with t = ~v and k = ~v" t k))
         (values h new-t)]))

(: bisimulate (Natural -> (Values (Omega-Hash Integer) (Omega-Tree Integer))))
(define (bisimulate n)
  (let: loop ([n n] [h empty-omega-hash] [t : (Omega-Tree Integer)  omega-leaf])
    (cond [(= n 0)  (values h t)]
          [else  (let-values ([(h t)  (bisimulation-step h t)])
                   (loop (- n 1) h t))])))

;; Test passes when no errors are raised
(call-with-values (λ () (bisimulate 10000)) void)
