#lang typed/racket

(require "../private/types.rkt")

;; ===================================================================================================
;; Randomized bisimulation tests for reffing and setting omega trees

(define-type (Omega-Hash A) (HashTable Omega-Idx A))

(define: empty-omega-hash : (Omega-Hash Integer)  (make-immutable-hash empty))

(: omega-tree->omega-hash ((Omega-Tree Integer) -> (Omega-Hash Integer)))
(define (omega-tree->omega-hash t)
  (make-immutable-hash
   (((inst omega-tree-map Integer (Pair Omega-Idx Integer)) 0) t (λ: ([k : Omega-Idx] [v : Integer])
                                                                   (cons k v)))))

(: omega-hash-ref ((Omega-Hash Integer) Omega-Idx -> Integer))
(define (omega-hash-ref h k)
  (hash-ref h k (λ () 0)))

(: omega-hash-set ((Omega-Hash Integer) Omega-Idx Integer -> (Omega-Hash Integer)))
(define (omega-hash-set h k i)
  (cond [(= i 0)  (hash-remove h k)]
        [else  (hash-set h k i)]))

(: random-omega-idx (-> Omega-Idx))
(define (random-omega-idx)
  (define d (expt 2 (+ 1 (random 3))))
  (/ (+ (random (- d 1)) 1) d))

(: bisimulation-step ((Omega-Hash Integer) (Omega-Tree Integer) -> (Values (Omega-Hash Integer)
                                                                           (Omega-Tree Integer))))
(define (bisimulation-step h t)
  (cond [((random) . < . 0.5)
         (define k (random-omega-idx))
         (define i (random 4))
         (define new-h (omega-hash-set h k i))
         (define new-t ((omega-tree-set 0) t k i))
         (unless (equal? new-h (omega-tree->omega-hash new-t))
           (error 'bisimulation-step "operation set ~v ~v failed with ~v and ~v" k i h t))
         (values new-h new-t)]
        [else
         (define k (random-omega-idx))
         (define i0 (omega-hash-ref h k))
         (define i1 ((omega-tree-ref 0) t k))
         (unless (= i0 i1)
           (error 'bisimulation-step "operation ref ~v failed with ~v and ~v" k h t))
         (define new-h (omega-hash-set h k 0))
         (define new-t ((omega-tree-set 0) t k 0))
         (unless (equal? new-h (omega-tree->omega-hash new-t))
           (error 'bisimulation-step "operation set ~v 0 failed with ~v and ~v" k h t))
         (values new-h new-t)]))

(: bisimulate (Natural -> (Values (Omega-Hash Integer) (Omega-Tree Integer))))
(define (bisimulate n)
  (let: loop ([n n] [h empty-omega-hash] [t : (Omega-Tree Integer)  omega-leaf])
    (cond [(= n 0)  (values h t)]
          [else  (let-values ([(h t)  (bisimulation-step h t)])
                   (loop (- n 1) h t))])))

;; Test passes when no errors are raised
(call-with-values (λ () (bisimulate 500)) void)
