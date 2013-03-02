#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list)

(provide (all-defined-out))

(define-type Omega-Idx (Listof (U 0 1)))

;; ===================================================================================================
;; Finitely restricted mappings from Omega-Idx

(define-type (Omega-Tree A) (U Omega-Leaf (Omega-Node A)))

(struct: Omega-Leaf () #:transparent)
(struct: (A) Omega-Node ([value : A]
                         [fst : (Omega-Tree A)]
                         [snd : (Omega-Tree A)])
  #:transparent)

(define omega-leaf (Omega-Leaf))
(define-syntax omega-leaf? (make-rename-transformer #'Omega-Leaf?))

(: omega-node (All (A) (A -> (A (Omega-Tree A) (Omega-Tree A) -> (Omega-Tree A)))))
(define ((omega-node default) v fst snd)
  (if (and (equal? v default) (omega-leaf? fst) (omega-leaf? snd))
      omega-leaf
      (Omega-Node v fst snd)))

(: omega-tree-value (All (A) (A -> ((Omega-Tree A) -> A))))
(define ((omega-tree-value default) t)
  (if (omega-leaf? t) default (Omega-Node-value t)))

(: omega-tree-fst (All (A) (case-> (Omega-Leaf -> Omega-Leaf)
                                   ((Omega-Tree A) -> (Omega-Tree A)))))
(define (omega-tree-fst t)
  (if (omega-leaf? t) t (Omega-Node-fst t)))

(: omega-tree-snd (All (A) (case-> (Omega-Leaf -> Omega-Leaf)
                                   ((Omega-Tree A) -> (Omega-Tree A)))))
(define (omega-tree-snd t)
  (if (omega-leaf? t) t (Omega-Node-snd t)))

(: omega-tree-ref (All (A) (A -> ((Omega-Tree A) Omega-Idx -> A))))
(define ((omega-tree-ref default) t r)
  (let: loop ([t t] [r r])
    (cond [(omega-leaf? t)  default]
          [(empty? r)  (Omega-Node-value t)]
          [(zero? (first r))
           (loop (Omega-Node-fst t) (rest r))]
          [else
           (loop (Omega-Node-snd t) (rest r))])))

(: omega-tree-set (All (A) (A -> ((Omega-Tree A) Omega-Idx A -> (Omega-Tree A)))))
(define (omega-tree-set default)
  (define make-node (omega-node default))
  (define get-value (omega-tree-value default))
  (λ (t r v)
    (let: loop ([t t] [r r])
      (define old-v (get-value t))
      (cond
        [(empty? r)
         (if (equal? v old-v) t (make-node v (omega-tree-fst t) (omega-tree-snd t)))]
        [(zero? (first r))
         (define fst (omega-tree-fst t))
         (define new-fst (loop fst (rest r)))
         (cond [(eq? fst new-fst)  t]
               [else  (make-node old-v new-fst (omega-tree-snd t))])]
        [else
         (define snd (omega-tree-snd t))
         (define new-snd (loop snd (rest r)))
         (cond [(eq? snd new-snd)  t]
               [else  (make-node old-v (omega-tree-fst t) new-snd)])]))))

(: omega-tree-join (All (A) (A (A A -> A) -> ((Omega-Tree A) (Omega-Tree A) -> (Omega-Tree A)))))
(define (omega-tree-join default join)
  (define make-node (omega-node default))
  (λ (t1 t2)
    (let: loop ([t1 t1] [t2 t2])
      (cond [(eq? t1 t2)  t1]
            [(or (omega-leaf? t1) (omega-leaf? t2))  omega-leaf]
            [else  (match-define (Omega-Node v1 fst1 snd1) t1)
                   (match-define (Omega-Node v2 fst2 snd2) t2)
                   (define v (join v1 v2))
                   (define fst (loop fst1 fst2))
                   (define snd (loop snd1 snd2))
                   (cond [(and (eq? fst fst1) (eq? snd snd1) (equal? v v1))  t1]
                         [(and (eq? fst fst2) (eq? snd snd2) (equal? v v2))  t2]
                         [else  (make-node v fst snd)])]))))

(: omega-tree-intersect
   (All (A E) (A (A A -> (U A E)) (Any -> Boolean : E)
                 -> ((Omega-Tree A) (Omega-Tree A) -> (U (Omega-Tree A) E)))))
(define (omega-tree-intersect default intersect empty-set?)
  (define make-node (omega-node default))
  (: loop ((Omega-Tree A) (Omega-Tree A) -> (U (Omega-Tree A) E)))
  (define (loop t1 t2)
    (cond [(eq? t1 t2)  t1]
          [(omega-leaf? t1)  t2]
          [(omega-leaf? t2)  t1]
          [else  (match-define (Omega-Node v1 fst1 snd1) t1)
                 (match-define (Omega-Node v2 fst2 snd2) t2)
                 (define v (intersect v1 v2))
                 (cond [(empty-set? v)  v]
                       [else
                        (define fst (loop fst1 fst2))
                        (cond [(empty-set? fst)  fst]
                              [else
                               (define snd (loop snd1 snd2))
                               (cond [(empty-set? snd)  snd]
                                     [(and (eq? fst fst1) (eq? snd snd1) (equal? v v1))  t1]
                                     [(and (eq? fst fst2) (eq? snd snd2) (equal? v v2))  t2]
                                     [else  (make-node v fst snd)])])])]))
  loop)

(: omega-tree-map (All (A B) (A -> ((Omega-Tree A) (A -> B) -> (Listof B)))))
(define ((omega-tree-map default) t f)
  (: bs (Listof B))
  (define bs
    (let: loop ([t t] [bs : (Listof B)  null])
      (cond [(omega-leaf? t)  bs]
            [else
             (define v (Omega-Node-value t))
             (let ([bs  (loop (Omega-Node-fst t) bs)])
               (cond [(equal? v default)  (loop (Omega-Node-snd t) bs)]
                     [else  (loop (Omega-Node-snd t) (cons (f v) bs))]))])))
  (reverse bs))

(: omega-tree->omega-tree (All (A B) (A B -> ((Omega-Tree A) (A -> B) -> (Omega-Tree B)))))
(define ((omega-tree->omega-tree a-default b-default) t f)
  (define make-node (omega-node b-default))
  (let: loop ([t t])
    (cond [(omega-leaf? t)  omega-leaf]
          [else
           (define a (Omega-Node-value t))
           (define b (if (equal? a a-default) b-default (f a)))
           (make-node b (loop (Omega-Node-fst t)) (loop (Omega-Node-snd t)))])))
