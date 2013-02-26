#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match)

(provide (all-defined-out))

(define-type Omega-Idx Nonnegative-Exact-Rational)

(: omega-expr-idx (Omega-Idx Omega-Idx -> Omega-Idx))
(define (omega-expr-idx r0 r1)
  (* 1/2 (+ r0 r1)))

(define-type Idx (U Symbol Natural Omega-Idx))

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
  (if (and (eq? v default) (omega-leaf? fst) (omega-leaf? snd))
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
  (let: loop ([t t] [r0 : Omega-Idx  0] [r1 : Omega-Idx  1])
    (cond [(omega-leaf? t)  default]
          [else
           (define mid (* 1/2 (+ r0 r1)))
           (cond [(r . < . mid)  (loop (Omega-Node-fst t) r0 mid)]
                 [(r . > . mid)  (loop (Omega-Node-snd t) mid r1)]
                 [else  (Omega-Node-value t)])])))

(: omega-tree-set (All (A) (A -> ((Omega-Tree A) Omega-Idx A -> (Omega-Tree A)))))
(define (omega-tree-set default)
  (define make-node (omega-node default))
  (define get-value (omega-tree-value default))
  (λ (t r v)
    (let: loop ([t t] [r0 : Omega-Idx  0] [r1 : Omega-Idx  1])
      (define old-v (get-value t))
      (define mid (/ (+ r0 r1) 2))
      (cond 
        [(r . < . mid)
         (define fst (omega-tree-fst t))
         (define new-fst (loop fst r0 mid))
         (cond [(eq? fst new-fst)  t]
               [else  (make-node old-v new-fst (omega-tree-snd t))])]
        [(r . > . mid)
         (define snd (omega-tree-snd t))
         (define new-snd (loop snd mid r1))
         (cond [(eq? snd new-snd)  t]
               [else  (make-node old-v (omega-tree-fst t) new-snd)])]
        [(eq? v old-v)  t]
        [else
         (make-node v (omega-tree-fst t) (omega-tree-snd t))]))))

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
                   (cond [(and (eq? v v1) (eq? fst fst1) (eq? snd snd1))  t1]
                         [(and (eq? v v2) (eq? fst fst2) (eq? snd snd2))  t2]
                         [else  (make-node v fst snd)])]))))

(: omega-tree-intersect
   (All (A E) (A (A A -> (U A E)) (Any -> Boolean : E)
                 -> ((Omega-Tree A) (Omega-Tree A) -> (U (Omega-Tree A) E)))))
(define (omega-tree-intersect default intersect empty-set?)
  (define make-node (omega-node default))
  (λ (t1 t2)
    (let: loop ([t1 t1] [t2 t2])
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
                                       [(and (eq? v v1) (eq? fst fst1) (eq? snd snd1))  t1]
                                       [(and (eq? v v2) (eq? fst fst2) (eq? snd snd2))  t2]
                                       [else  (make-node v fst snd)])])])]))))

(: omega-tree-map (All (A B) (A -> ((Omega-Tree A) (Omega-Idx A -> B) -> (Listof B)))))
(define ((omega-tree-map default) t f)
  (: bs (Listof B))
  (define bs
    (let: loop ([t t] [r0 : Omega-Idx  0] [r1 : Omega-Idx  1] [bs : (Listof B)  null])
      (define mid (* 1/2 (+ r0 r1)))
      (cond [(omega-leaf? t)  bs]
            [else
             (define v (Omega-Node-value t))
             (let ([bs  (loop (Omega-Node-fst t) r0 mid bs)])
               (cond [(eq? v default)  (loop (Omega-Node-snd t) mid r1 bs)]
                     [else  (loop (Omega-Node-snd t) mid r1 (cons (f mid v) bs))]))])))
  (reverse bs))

(: omega-tree-keys (All (A) (A -> ((Omega-Tree A) -> (Listof Omega-Idx)))))
(define ((omega-tree-keys default) t)
  (((inst omega-tree-map A Omega-Idx) default) t (λ: ([k : Omega-Idx] [_ : A]) k)))
