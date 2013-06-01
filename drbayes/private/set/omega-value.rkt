#lang typed/racket/base

(require racket/promise
         racket/list
         racket/match
         "../untyped-utils.rkt")

(provide (all-defined-out))

(define-type Omega-Index (Listof (U 0 1)))
(define-type Nonempty-Omega-Index (Pair (U 0 1) Omega-Index))


(define-singleton-type Random-Value random-value)

(struct: (V) Tree-Node ([value : (Boxof (U V Random-Value))] [children : (Tree-Children V)])
  #:transparent)
(struct: (V) Tree-Children ([fst : (Tree V)] [snd : (Tree V)]) #:transparent)
(define-type (Tree V) (Boxof (U Random-Value (Tree-Node V))))

(: random-tree (All (V) (-> (Tree V))))
(define (random-tree)
  ((inst box (U (Tree-Node V) Random-Value)) random-value))

(: random-tree-children (All (V) (-> (Tree-Children V))))
(define (random-tree-children)
  (Tree-Children ((inst random-tree V)) ((inst random-tree V))))

(: unbox-random-tree (All (V) ((Tree V) -> (Tree-Node V))))
(define (unbox-random-tree bx)
  (define t (unbox bx))
  (cond [(random-value? t)  (define t (Tree-Node ((inst box (U V Random-Value)) random-value)
                                                 ((inst random-tree-children V))))
                            (set-box! bx t)
                            t]
        [else  t]))

(: unbox-random-value (All (V) ((-> V) (Boxof (U V Random-Value)) -> V)))
(define (unbox-random-value random bx)
  (define v (unbox bx))
  (cond [(random-value? v)  (define v (random))
                            (set-box! bx v)
                            v]
        [else  v]))

(: make-tree-value (All (V) ((-> V) -> ((Tree V) -> V))))
(define ((make-tree-value random) bx)
  (unbox-random-value random (Tree-Node-value (unbox-random-tree bx))))

(: tree-children (All (V) ((Tree V) -> (Tree-Children V))))
(define (tree-children bx)
  (Tree-Node-children (unbox-random-tree bx)))

(: make-tree-ref (All (V) ((-> V) -> ((Tree V) Omega-Index -> V))))
(define (make-tree-ref random)
  (define tree-value (make-tree-value random))
  
  (: tree-ref ((Tree V) Omega-Index -> V))
  (define (tree-ref t r)
    (cond [(empty? r)  (tree-value t)]
          [(zero? (first r))  (tree-ref (Tree-Children-fst (tree-children t)) (rest r))]
          [else               (tree-ref (Tree-Children-snd (tree-children t)) (rest r))]))
  
  tree-ref)

(: tree-value! (All (V) ((Tree V) V -> V)))
(define (tree-value! t v)
  (define bx (Tree-Node-value (unbox-random-tree t)))
  (define v0 (unbox bx))
  (cond [(random-value? v0)  (set-box! bx v)
                             v]
        [else  v0]))

(: tree-ref! (All (V) ((Tree V) Omega-Index V -> V)))
(define (tree-ref! t r v)
  (cond [(empty? r)  (tree-value! t v)]
        [(zero? (first r))  (tree-ref! (Tree-Children-fst (tree-children t)) (rest r) v)]
        [else               (tree-ref! (Tree-Children-snd (tree-children t)) (rest r) v)]))

(: tree->list (All (V) ((Tree V) -> (Listof V))))
(define (tree->list bx)
  (: xs (Listof V))
  (define xs
    (let: loop ([bx bx] [xs : (Listof V)  null])
      (define t (unbox bx))
      (cond [(random-value? t)  xs]
            [else
             (match-define (Tree-Node x-bx (Tree-Children t1 t2)) (unbox-random-tree bx))
             (let ([xs  (loop t1 xs)])
               (define x (unbox x-bx))
               (cond [(random-value? x)  (loop t2 xs)]
                     [else  (loop t2 (cons x xs))]))])))
  (reverse xs))

;; ===================================================================================================
;; Omega

(define-type Omega (Tree Flonum))
(define-type Omega-Children (Tree-Children Flonum))

(define random-omega (inst random-tree Flonum))
(define random-omega-children (inst random-tree-children Flonum))
(define omega-value (make-tree-value random))
(define omega-children (inst tree-children Flonum))
(define omega-ref (make-tree-ref random))
(define omega->list (inst tree->list Flonum))

;; ===================================================================================================
;; Branches

(define (random-boolean) ((random) . > . 0.5))

(define-type Branches (Tree Boolean))
(define-type Branches-Children (Tree-Children Boolean))

(define random-branches (inst random-tree Boolean))
(define random-branches-children (inst random-tree-children Boolean))
(define branches-value (make-tree-value random-boolean))
(define branches-children (inst tree-children Boolean))
(define branches-ref (make-tree-ref random-boolean))
(define branches-ref! (inst tree-ref! Boolean))
(define branches->list (inst tree->list Boolean))
