#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         "extremal-set.rkt"
         "interval.rkt"
         "boolean-rect.rkt")

(provide (all-defined-out))

(define-type Omega-Index (Listof (U 0 1)))

;; ===================================================================================================
;; Finitely restricted mappings from Omega-Index

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

(: omega-tree-ref (All (A) (A -> ((Omega-Tree A) Omega-Index -> A))))
(define ((omega-tree-ref default) t r)
  (let: loop ([t t] [r r])
    (cond [(omega-leaf? t)  default]
          [(empty? r)  (Omega-Node-value t)]
          [(zero? (first r))
           (loop (Omega-Node-fst t) (rest r))]
          [else
           (loop (Omega-Node-snd t) (rest r))])))

(: omega-tree-set (All (A) (A -> ((Omega-Tree A) Omega-Index A -> (Omega-Tree A)))))
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

;; ===================================================================================================
;; Infinite product space values

(define-type Omega-Hash (Omega-Tree Flonum))

(struct: Omega ([hash : (Boxof Omega-Hash)])
  #:transparent)

(define-syntax omega? (make-rename-transformer #'Omega?))
(define-syntax omega-hash (make-rename-transformer #'Omega-hash))

(define omega-hash-default +nan.0)
(define omega-hash-ref ((inst omega-tree-ref Flonum) omega-hash-default))
(define omega-hash-set ((inst omega-tree-set Flonum) omega-hash-default))

(: omega-ref (Omega Omega-Index -> Flonum))
(define (omega-ref ω k)
  (define h (omega-hash ω))
  (define x (omega-hash-ref (unbox h) k))
  (cond [(rational? x)  x]
        [else  (define x (random))
               (set-box! h (omega-hash-set (unbox h) k x))
               x]))

(: omega-map (All (B) (Omega (Flonum -> B) -> (Listof B))))
(define (omega-map ω f)
  (((inst omega-tree-map Flonum B) omega-hash-default) (unbox (Omega-hash ω)) f))

;; ===================================================================================================
;; Infinite product space rectangles

(define-type Omega-Rect (Omega-Tree Interval))
(define-type Maybe-Omega-Rect (U Empty-Set Omega-Rect))

(define omega-nonempty?
  (λ: ([Ω : Maybe-Omega-Rect]) (not (empty-set? Ω))))

(define omega-rect-ref ((inst omega-tree-ref Interval) unit-interval))
(define omega-rect-set ((inst omega-tree-set Interval) unit-interval))

(: omega-rect-fst (Omega-Rect -> Omega-Rect))
(define omega-rect-fst omega-tree-fst)

(: omega-rect-snd (Omega-Rect -> Omega-Rect))
(define omega-rect-snd omega-tree-snd)

(: omega-rect-value (Omega-Rect -> Interval))
(define omega-rect-value (omega-tree-value unit-interval))

(: omega-rect Omega-Rect)
(define omega-rect omega-leaf)

(: omega-rect-map (All (B) (Omega-Rect (Interval -> B) -> (Listof B))))
(define (omega-rect-map Ω f)
  (((inst omega-tree-map Interval B) unit-interval) Ω f))

(define just-omega-rect-node ((inst omega-node Interval) unit-interval))
(define just-omega-rect-join ((inst omega-tree-join Interval) unit-interval interval-join))
(define just-omega-rect-intersect
  ((inst omega-tree-intersect Interval Empty-Set) unit-interval interval-intersect empty-set?))

(: omega-rect-node (Interval Maybe-Omega-Rect Maybe-Omega-Rect -> Maybe-Omega-Rect))
(define (omega-rect-node I Ω1 Ω2)
  (cond [(empty-set? Ω1)  Ω1]
        [(empty-set? Ω2)  Ω2]
        [else  (just-omega-rect-node I Ω1 Ω2)]))

(: omega-rect-node/last (Omega-Rect Interval Maybe-Omega-Rect Maybe-Omega-Rect -> Maybe-Omega-Rect))
(define (omega-rect-node/last Ω I Ω1 Ω2)
  (cond [(and (equal? I (omega-rect-value Ω))
              (eq? Ω1 (omega-rect-fst Ω))
              (eq? Ω2 (omega-rect-snd Ω)))
         Ω]
        [else  (unit-omega-rect-node Ω1 Ω2)]))

(: unit-omega-rect-node (Maybe-Omega-Rect Maybe-Omega-Rect -> Maybe-Omega-Rect))
(define (unit-omega-rect-node Ω1 Ω2)
  (omega-rect-node unit-interval Ω1 Ω2))

(: unit-omega-rect-node/last (Omega-Rect Maybe-Omega-Rect Maybe-Omega-Rect -> Maybe-Omega-Rect))
(define (unit-omega-rect-node/last Ω Ω1 Ω2)
  (cond [(and (eq? Ω1 (omega-rect-fst Ω)) (eq? Ω2 (omega-rect-snd Ω)))  Ω]
        [else  (unit-omega-rect-node Ω1 Ω2)]))

(: omega-rect-join (Maybe-Omega-Rect Maybe-Omega-Rect -> Maybe-Omega-Rect))
(define (omega-rect-join Ω1 Ω2)
  (cond [(empty-set? Ω1)  Ω2]
        [(empty-set? Ω2)  Ω1]
        [else  (just-omega-rect-join Ω1 Ω2)]))

(: omega-rect-intersect (Maybe-Omega-Rect Maybe-Omega-Rect -> Maybe-Omega-Rect))
(define (omega-rect-intersect Ω1 Ω2)
  (cond [(empty-set? Ω1)  Ω1]
        [(empty-set? Ω2)  Ω2]
        [else  (just-omega-rect-intersect Ω1 Ω2)]))

(define omega-rect->omega-hash
  ((inst omega-tree->omega-tree Interval Flonum) unit-interval omega-hash-default))

(: omega-rect-sample-point (Omega-Rect -> Omega))
(define (omega-rect-sample-point Ω)
  (Omega (box (omega-rect->omega-hash Ω interval-sample-point))))

(: omega-rect-measure (Omega-Rect -> Flonum))
(define (omega-rect-measure Ω)
  (real->double-flonum (apply * (omega-rect-map Ω interval-measure))))

;; ===================================================================================================
;; Conditional bisection rectangles

(define-type Branches-Rect (Omega-Tree Boolean-Rect))
(define-type Maybe-Branches-Rect (U Empty-Set Branches-Rect))

(define branches-rect-ref ((inst omega-tree-ref Boolean-Rect) 'tf))
(define branches-rect-set ((inst omega-tree-set Boolean-Rect) 'tf))

(: branches-rect-fst (Branches-Rect -> Branches-Rect))
(define branches-rect-fst omega-tree-fst)

(: branches-rect-snd (Branches-Rect -> Branches-Rect))
(define branches-rect-snd omega-tree-snd)

(: branches-rect-value (Branches-Rect -> Boolean-Rect))
(define branches-rect-value (omega-tree-value 'tf))

(: branches-rect Branches-Rect)
(define branches-rect omega-leaf)

(define just-branches-rect-node ((inst omega-node Boolean-Rect) 'tf))
(define just-branches-rect-join ((inst omega-tree-join Boolean-Rect) 'tf boolean-rect-join))
(define just-branches-rect-intersect
  ((inst omega-tree-intersect Boolean-Rect Empty-Set) 'tf boolean-rect-intersect empty-set?))

(: branches-rect-node
   (Boolean-Rect Maybe-Branches-Rect Maybe-Branches-Rect -> Maybe-Branches-Rect))
(define (branches-rect-node b Z1 Z2)
  (cond [(empty-set? Z1)  Z1]
        [(empty-set? Z2)  Z2]
        [else  (just-branches-rect-node b Z1 Z2)]))

(: branches-rect-node/last
   (Branches-Rect Boolean-Rect Maybe-Branches-Rect Maybe-Branches-Rect -> Maybe-Branches-Rect))
(define (branches-rect-node/last Z b Z1 Z2)
  (cond [(and (eq? b (branches-rect-value Z))
              (eq? Z1 (branches-rect-fst Z))
              (eq? Z2 (branches-rect-snd Z)))
         Z]
        [else  (branches-rect-node b Z1 Z2)]))

(: branches-rect-join (Maybe-Branches-Rect Maybe-Branches-Rect -> Maybe-Branches-Rect))
(define (branches-rect-join Z1 Z2)
  (cond [(empty-set? Z1)  Z2]
        [(empty-set? Z2)  Z1]
        [else  (just-branches-rect-join Z1 Z2)]))

(: branches-rect-intersect (Maybe-Branches-Rect Maybe-Branches-Rect -> Maybe-Branches-Rect))
(define (branches-rect-intersect Z1 Z2)
  (cond [(empty-set? Z1)  Z1]
        [(empty-set? Z2)  Z2]
        [else  (just-branches-rect-intersect Z1 Z2)]))
