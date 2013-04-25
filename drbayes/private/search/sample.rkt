#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         math/flonum
         math/distributions
         "../set.rkt"
         "../arrow.rkt"
         "search.rkt"
         "../utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(define-type Omega-Pair (Pair Omega-Rect Branches-Rect))
(define-type Maybe-Omega-Pair (U Empty-Set Omega-Pair))
(define-type Omega-Search-Tree (Search-Tree Maybe-Omega-Pair))

(struct: omega-rect-sample ([Ω : Omega-Rect] [Z : Branches-Rect] [measure : Flonum] [prob : Flonum])
  #:transparent)

(struct: omega-sample ([ω : Omega] [z : Branches] [prob : Flonum])
  #:transparent)

(define-type Ann-Index (U ann-interval-index ann-if-indexes))
(define-type Ann-Indexes (Listof Ann-Index))

(struct: ann-interval-index interval-index
  ([num-splits : Natural]
   [min-length : Flonum])
  #:transparent)

(struct: ann-if-indexes
  ([index : Omega-Index]
   [true : (Promise Ann-Indexes)]
   [false : (Promise Ann-Indexes)])
  #:transparent)

(: annotate-indexes (Indexes -> Ann-Indexes))
(define (annotate-indexes idxs)
  (define m (interval-max-splits))
  (define l (interval-min-length))
  (let loop ([idxs idxs])
    (cond
      [(empty? idxs)  idxs]
      [(if-indexes? (first idxs))
       (match-define (if-indexes i t-idxs f-idxs) (first idxs))
       (cons (ann-if-indexes i (delay (loop (t-idxs))) (delay (loop (f-idxs))))
             (loop (rest idxs)))]
      [else
       (match-define (interval-index i split) (first idxs))
       (cons (ann-interval-index i split m l) (loop (rest idxs)))])))

(: interval-split (Interval Flonum -> (Values (Listof Interval) (Listof Flonum))))
(define (interval-split I p)
  (match-define (interval a b a? b?) I)
  (define c (fl* p (fl+ a b)))
  (define m1 (fl- c a))
  (define m2 (fl- b c))
  (if (and (positive? m1) (positive? m2))
      (values (list (Interval a c a? #t) (Interval c b #f b?)) (list m1 m2))
      (values (list I) (list 1.0))))

;; ===================================================================================================

(define-type Refiner (Omega-Rect Branches-Rect -> (Values Maybe-Omega-Rect Maybe-Branches-Rect)))

(: preimage-refiner (Rand-Computation Nonempty-Set -> Refiner))
(define ((preimage-refiner e-comp K) Ω Z)
  (define e-meaning (e-comp Ω Z null-rect))
  (cond
    [(empty-meaning? e-meaning)  (values empty-set empty-set)]
    [else
     (match-define (rand-computation-meaning Ze Ke e-pre) e-meaning)
     (let*-values ([(Z)  (branches-rect-intersect Z Ze)]
                   [(K)  (set-intersect K Ke)]
                   [(Ω Z Γ)  (run-rand-preimage e-pre Z K)])
       (cond [(not (or (empty-set? Γ) (null-rect? Γ)))
              (raise-result-error 'preimage-refiner "(U Empty-Set Null-Rect)" Γ)]
             [(or (empty-set? Ω) (empty-set? Z))  (values empty-set empty-set)]
             [else  (values Ω Z)]))]))

(: refinement-sample* (Omega-Rect Branches-Rect Indexes Refiner Natural
                                  -> (Listof omega-rect-sample)))
(define (refinement-sample* Ω Z idxs refine n)
  (define t (build-search-tree Ω Z (annotate-indexes idxs) refine))
  (define-values (ts ps _t _q) (sample-search-tree* t n))
  (let loop ([ts ts] [ps ps])
    (cond [(or (empty? ts) (empty? ps))  empty]
          [else
           (match-define (search-leaf ΩZ m) (first ts))
           (define p (first ps))
           (cond [(empty-set? ΩZ)  (loop (rest ts) (rest ps))]
                 [else
                  (match-define (cons Ω Z) ΩZ)
                  (cons (omega-rect-sample Ω Z m p) (loop (rest ts) (rest ps)))])])))

(: expand-ann-indexes (Omega-Rect Branches-Rect Ann-Indexes Refiner -> Ann-Indexes))
(define (expand-ann-indexes Ω Z idxs refine)
  (let loop ([idxs idxs])
    (cond
      [(empty? idxs)  idxs]
      [else
       (define idx (first idxs))
       (define rest-idxs (rest idxs))
       (cond
         [(ann-if-indexes? idx)
          (match-define (ann-if-indexes i t-idxs f-idxs) idx)
          (define b (branches-rect-ref Z i))
          (cond [(eq? b trues)   (append (force t-idxs) (loop rest-idxs))]
                [(eq? b falses)  (append (force f-idxs) (loop rest-idxs))]
                [else  (cons idx (loop rest-idxs))])]
         [else
          (cons idx (loop rest-idxs))])])))

(: ann-index< (Omega-Rect Branches-Rect Refiner -> (Ann-Index Ann-Index -> Boolean)))
(define ((ann-index< Ω Z refine) idx0 idx1)
  (cond [(ann-if-indexes? idx0)  #f]
        [(ann-if-indexes? idx1)  #t]
        [else
         (match-define (ann-interval-index i0 split0 m0 l0) idx0)
         (match-define (ann-interval-index i1 split1 m1 l1) idx1)
         #;
         ((interval*-measure (omega-rect-ref Ω i0)) . > . (interval*-measure (omega-rect-ref Ω i1)))
         
         (m0 . > . m1)]))

(: choose-ann-index (Omega-Rect Branches-Rect Ann-Indexes Refiner -> (Values Ann-Index Ann-Indexes)))
(define (choose-ann-index Ω Z idxs refine)
  (let ([idxs  (sort idxs (ann-index< Ω Z refine))])
    (values (first idxs) (rest idxs))))

(: build-search-tree (Omega-Rect Branches-Rect Ann-Indexes Refiner -> Omega-Search-Tree))
(define (build-search-tree Ω Z idxs refine)
  (cond
    [(or (empty-set? Ω) (empty-set? Z))  (search-leaf empty-set 0.0)]
    [else
     (let ([idxs  (expand-ann-indexes Ω Z idxs refine)])
       (cond [(empty? idxs)  (search-leaf (cons Ω Z) (omega-rect-measure Ω))]
             [else
              (let-values ([(idx idxs)  (choose-ann-index Ω Z idxs refine)])
                (if (ann-if-indexes? idx)
                    (build-search-tree/if Ω Z idx idxs refine)
                    (build-search-tree/ivl Ω Z idx idxs refine)))]))]))

(: build-search-tree/if (Omega-Rect Branches-Rect ann-if-indexes Ann-Indexes Refiner
                                    -> Omega-Search-Tree))
(define (build-search-tree/if Ω Z idx idxs refine)
  (match-define (ann-if-indexes i t-idxs f-idxs) idx)
  
  (: make-node (Boolean-Rect (Promise Ann-Indexes) -> Omega-Search-Tree))
  (define (make-node b b-idxs)
    (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z i b))])
      (cond [(or (empty-set? Ω) (empty-set? Z))  (search-leaf empty-set 0.0)]
            [else  (build-search-tree Ω Z (append (force b-idxs) idxs) refine)])))
  
  (define b (branches-rect-ref Z i))
  (cond [(eq? b trues)   (make-node trues t-idxs)]
        [(eq? b falses)  (make-node falses f-idxs)]
        [else
         (search-node (list (delay (make-node trues t-idxs))
                            (delay (make-node falses f-idxs)))
                      (list 0.5 0.5)
                      'nondeterministic
                      'branches)]))

(: proportional-split (Omega-Rect Branches-Rect Refiner Omega-Index Interval
                               -> (Values (Listof Interval) (Listof Flonum))))
(define (proportional-split Ω Z refine i I)
  (define-values (Is ls) (interval-split I 0.5))
  (match Is
    [(list I0 I1)
     (define-values (Ω0 Z0) (refine (omega-rect-set Ω i I0) Z))
     (define-values (Ω1 Z1) (refine (omega-rect-set Ω i I1) Z))
     (define m0 (if (or (empty-set? Ω0) (empty-set? Z0)) 0.0 (omega-rect-measure Ω0)))
     (define m1 (if (or (empty-set? Ω1) (empty-set? Z1)) 0.0 (omega-rect-measure Ω1)))
     (cond [(and (positive? m0) (positive? m1))
            #;(values Is (list m0 m1))
            (values Is (map + (normalize-probs ls) (normalize-probs (list m0 m1))))
            #;(values Is ls)]
           [(positive? m0)  (values (list I0) (list 1.0))]
           [(positive? m1)  (values (list I1) (list 1.0))]
           [else  (values Is ls)])]
    [_  (values Is ls)]))

(: build-search-tree/ivl (Omega-Rect Branches-Rect ann-interval-index Ann-Indexes Refiner
                                     -> Omega-Search-Tree))
(define (build-search-tree/ivl Ω Z idx idxs refine)
  (match-define (ann-interval-index i split m min-length) idx)
  (cond
    [(zero? m)  (build-search-tree Ω Z idxs refine)]
    [else
     (define I (omega-rect-ref Ω i))
     (define-values (Is ls)
       (cond [(interval-list? I)  (define Is (interval-list-elements I))
                                  (values Is (map interval-measure Is))]
             [((interval-measure I) . < . min-length)  (values (list I) (list 1.0))]
             [split  (split I)]
             ;[else   (interval-split I 0.5)]
             [else   (proportional-split Ω Z refine i I)]
             ))
     (cond
       [(or (empty? Is) (empty? ls))
        (build-search-tree Ω Z idxs refine)]
       [(or (empty? (rest Is)) (empty? (rest ls)))
        (let-values ([(Ω Z)  (refine (omega-rect-set Ω i (first Is)) Z)])
          (cond [(or (empty-set? Ω) (empty-set? Z))  (search-leaf empty-set 0.0)]
                [else
                 (define idx (ann-interval-index i split (- m 1) min-length))
                 (build-search-tree Ω Z (cons idx idxs) refine)]))]
       [else
        (: make-node (Interval -> (Promise Omega-Search-Tree)))
        (define (make-node I)
          (delay
            (let-values ([(Ω Z)  (refine (omega-rect-set Ω i I) Z)])
              (cond [(or (empty-set? Ω) (empty-set? Z))  (search-leaf empty-set 0.0)]
                    [else
                     (define idx (ann-interval-index i split (- m 1) min-length))
                     (build-search-tree Ω Z (cons idx idxs) refine)]))))
        
        (search-node (map/+2 make-node Is)
                     (normalize-probs/+2 ls)
                     'probabilistic
                     'splits)])]))

;; ===================================================================================================

(: refinement-sample (Maybe-Omega-Rect Maybe-Branches-Rect Flonum Flonum Indexes Refiner
                                             -> (U #f omega-rect-sample)))
(define (refinement-sample Ω Z m p idxs refine)
  (cond
    [(or (empty-set? Ω) (empty-set? Z))  #f]
    [(empty? idxs)  (omega-rect-sample Ω Z m p)]
    [(if-indexes? (first idxs))
     (refinement-sample/if Ω Z m p (first idxs) (rest idxs) refine)]
    [else
     (refinement-sample/ivl Ω Z m p (first idxs) (rest idxs) refine)]))

(: refinement-sample/if (Omega-Rect Branches-Rect Flonum Flonum if-indexes Indexes Refiner
                                    -> (U #f omega-rect-sample)))
(define (refinement-sample/if Ω Z m p idx idxs refine)
  (match-define (if-indexes i t-idxs f-idxs) idx)
  (define b (branches-rect-ref Z i))
  (cond [(eq? b trues)   (refinement-sample Ω Z m p (append (t-idxs) idxs) refine)]
        [(eq? b falses)  (refinement-sample Ω Z m p (append (f-idxs) idxs) refine)]
        [else
         (define-values (b new-idxs q)
           (cond [((random) . < . 0.5)  (values trues (t-idxs) 0.5)]
                 [else  (values falses (f-idxs) 0.5)]))
         (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z i b))])
           (refinement-sample Ω Z m (* p q) (append new-idxs idxs) refine))]))

(: refinement-sample/ivl (Omega-Rect Branches-Rect Flonum Flonum interval-index Indexes Refiner
                                     -> (U #f omega-rect-sample)))
(define (refinement-sample/ivl Ω Z m p idx idxs refine)
  (match-define (interval-index i split) idx)
  (define I (omega-rect-ref Ω i))
  (define x (interval*-sample-point I))
  (define J (Interval x x #t #t))
  (define q (interval*-measure I))
  (let-values ([(Ω Z)  (refine (omega-rect-set Ω i J) Z)])
    (refinement-sample Ω Z (* m q) p idxs refine)))

;; ===================================================================================================

(: refinement-sample-point (Omega-Rect Branches-Rect Indexes Refiner -> (U #f omega-sample)))
(define (refinement-sample-point Ω Z idxs refine)
  (match (refinement-sample Ω Z 1.0 1.0 idxs refine)
    [(omega-rect-sample Ω Z m p)
     (define ω (omega-rect-sample-point Ω))
     (define z (branches-rect-sample-point Z))
     (omega-sample ω z (/ m p))]
    [_  #f]))

;; ===================================================================================================
;; Front end to sampler

(: drbayes-sample (case-> (Expression Natural -> (Values (Listof Value) (Listof Flonum)))
                          (Expression Natural Set -> (Values (Listof Value) (Listof Flonum)))))
(define (drbayes-sample f n [K universe])
  (match-define (rand-expression-meaning idxs f-fwd f-comp) (run-rand-expression (->rand f)))
  
  (define (empty-set-error)
    (error 'drbayes-sample "cannot sample from the empty set"))
  
  (define refine
    (cond [(empty-set? K)  (empty-set-error)]
          [else  (preimage-refiner f-comp K)]))
  
  (define-values (Ω Z)
    (let-values ([(Ω Z)  (refine omega-rect branches-rect)])
      (if (or (empty-set? Ω) (empty-set? Z)) (empty-set-error) (values Ω Z))))
  
  (define t (build-search-tree Ω Z (annotate-indexes idxs) refine))
  
  (let: loop ([i : Natural  0]
              [ks : (Listof Value)   empty]
              [ws : (Listof Flonum)  empty]
              [t : Omega-Search-Tree  t]
              [q : Flonum  1.0])
    (cond
      [(and (i . < . n) (q . > . 0.0))
       (let ([i  (+ i 1)])
         (when (= 0 (remainder i 100))
           (printf "i = ~v~n" i)
           (flush-output))
         (let-values ([(leaf-ts leaf-ps t q)  (sample-search-tree t q)])
           (let: inner-loop ([leaf-ts leaf-ts]
                             [leaf-ps leaf-ps]
                             [ks : (Listof Value)   ks]
                             [ws : (Listof Flonum)  ws])
             (cond
               [(or (empty? leaf-ts) (empty? leaf-ps))
                (loop i ks ws t q)]
               [else
                (define leaf-t (first leaf-ts))
                (define leaf-p (first leaf-ps))
                (match-define (search-leaf ΩZ m) leaf-t)
                (cond
                  [(or (empty-set? ΩZ) (m . <= . 0.0))
                   (printf "sample-search-tree returned failure~n")
                   (inner-loop (rest leaf-ts) (rest leaf-ps) ks ws)]
                  [else
                   (match-define (cons Ω Z) ΩZ)
                   (define pt (refinement-sample-point Ω Z idxs refine))
                   ;(define ω (omega-rect-sample-point Ω))
                   ;(define z (branches-rect-sample-point Z))
                   ;(define pt (omega-sample ω z m))
                   (match pt
                     [(omega-sample ω z m)
                      (define k (f-fwd ω z null))
                      (cond [(and (not (bottom? k)) (set-member? K k))
                             (printf "success!~n")
                             (inner-loop (rest leaf-ts) (rest leaf-ps)
                                         (cons k ks) (cons (/ m leaf-p) ws))]
                            [(bottom? k)
                             (printf "k = bottom: ~a~n" (force (bottom-message k)))
                             (inner-loop (rest leaf-ts) (rest leaf-ps) ks ws)]
                            [else
                             (printf "k not in K~n")
                             (inner-loop (rest leaf-ts) (rest leaf-ps) ks ws)])]
                     [_
                      (printf "refinement-sample-point returned #f~n")
                      (inner-loop (rest leaf-ts) (rest leaf-ps) ks ws)])])]))))]
      [else
       (when (q . <= . 0.0)
         (printf "bailing because top probability is zero~n"))
       (values ks ws)])))
