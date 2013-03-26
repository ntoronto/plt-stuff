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
(define-type Omega-Search-Tree (Search-Tree Omega-Pair))

(struct: omega-rect-sample ([Ω : Omega-Rect] [Z : Branches-Rect] [measure : Flonum] [prob : Flonum])
  #:transparent)

(struct: omega-sample ([ω : Omega] [z : Branches] [prob : Flonum])
  #:transparent)

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
             [else
              (values Ω Z)]))]))

(: refinement-sample* (Omega-Rect Branches-Rect Indexes Refiner Natural
                                  -> (Listof omega-rect-sample)))
(define (refinement-sample* Ω Z idxs refine n)
  (define t (build-search-tree Ω Z idxs refine))
  (define-values (ts ps) (sample-search-tree* t n))
  (map (λ: ([t : (success-leaf Omega-Pair)] [p : Flonum])
         (match-define (success-leaf (cons Ω Z) m) t)
         (omega-rect-sample Ω Z m p))
       ts ps))

(: build-search-tree (Maybe-Omega-Rect Maybe-Branches-Rect Indexes Refiner -> Omega-Search-Tree))
(define (build-search-tree Ω Z idxs refine)
  (cond
    [(or (empty-set? Ω) (empty-set? Z))  (failure-leaf)]
    [(empty? idxs)  (success-leaf (cons Ω Z) (omega-rect-measure Ω))]
    [(if-indexes? (first idxs))
     (build-search-tree/if Ω Z (first idxs) (rest idxs) refine)]
    [else
     (build-search-tree/ivl Ω Z (first idxs) (rest idxs) refine)]))

(: build-search-tree/if (Omega-Rect Branches-Rect if-indexes Indexes Refiner
                                    -> Omega-Search-Tree))
(define (build-search-tree/if Ω Z idx idxs refine)
  (match-define (if-indexes i t-idxs f-idxs) idx)
  
  (: make-node (Boolean-Rect (-> Indexes) -> Omega-Search-Tree))
  (define (make-node b b-idxs)
    (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z i b))])
      (build-search-tree Ω Z (append (b-idxs) idxs) refine)))
  
  (define b (branches-rect-ref Z i))
  (cond [(eq? b trues)   (make-node trues t-idxs)]
        [(eq? b falses)  (make-node falses f-idxs)]
        [else  (search-node (list (delay (make-node trues t-idxs))
                                  (delay (make-node falses f-idxs)))
                            (list 0.5 0.5)
                            'branches)]))

(: build-search-tree/ivl (Omega-Rect Branches-Rect interval-index Indexes Refiner
                                     -> Omega-Search-Tree))
(define (build-search-tree/ivl Ω Z idx idxs refine)
  (match-define (interval-index i split m min-length) idx)
  (cond
    [(zero? m)  (build-search-tree Ω Z idxs refine)]
    [else
     (define I (omega-rect-ref Ω i))
     (define-values (Is ls) (split I min-length))
     (cond
       [(or (empty? Is) (empty? ls))
        (build-search-tree Ω Z idxs refine)]
       [(or (empty? (rest Is)) (empty? (rest ls)))
        (let-values ([(Ω Z)  (refine (omega-rect-set Ω i (first Is)) Z)])
          (build-search-tree Ω Z idxs refine))]
       [else
        (: make-node (Interval -> (Promise Omega-Search-Tree)))
        (define (make-node I)
          (delay (let-values ([(Ω Z)  (refine (omega-rect-set Ω i I) Z)]
                              [(idx)  (interval-index i split (- m 1) min-length)])
                   (build-search-tree Ω Z (cons idx idxs) refine))))
        
        (search-node (map/+2 make-node Is) (normalize-probs/+2 ls) 'splits)])]))

;; ===================================================================================================

(: refinement-sample-point (Maybe-Omega-Rect Maybe-Branches-Rect Flonum Indexes Refiner
                                             -> (U #f omega-sample)))
(define (refinement-sample-point Ω Z prob idxs refine)
  (cond
    [(or (empty-set? Ω) (empty-set? Z))  #f]
    [(empty? idxs)  (omega-sample (omega-rect-sample-point Ω)
                                  (branches-rect-sample-point Z)
                                  prob)]
    [(if-indexes? (first idxs))
     (refinement-sample-point/if Ω Z prob (first idxs) (rest idxs) refine)]
    [else
     (refinement-sample-point/ivl Ω Z prob (first idxs) (rest idxs) refine)]))

(: refinement-sample-point/if (Omega-Rect Branches-Rect Flonum if-indexes Indexes Refiner
                                          -> (U #f omega-sample)))
(define (refinement-sample-point/if Ω Z prob idx idxs refine)
  (match-define (if-indexes i t-idxs f-idxs) idx)
  
  (define b (branches-rect-ref Z i))
  (cond [(eq? b trues)   (refinement-sample-point Ω Z prob (append (t-idxs) idxs) refine)]
        [(eq? b falses)  (refinement-sample-point Ω Z prob (append (f-idxs) idxs) refine)]
        [else  (define b (if ((random) . < . 0.5) trues falses))
               (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z i b))])
                 (refinement-sample-point Ω Z (* prob 0.5) (cons idx idxs) refine))]))

(: refinement-sample-point/ivl (Omega-Rect Branches-Rect Flonum interval-index Indexes Refiner
                                           -> (U #f omega-sample)))
(define (refinement-sample-point/ivl Ω Z prob idx idxs refine)
  (match-define (interval-index i split m min-length) idx)
  
  (define I (omega-rect-ref Ω i))
  (define x (interval-sample-point I))
  (let-values ([(Ω Z)  (refine (omega-rect-set Ω i (Interval x x #t #t)) Z)])
    (refinement-sample-point Ω Z (* prob (interval-measure I)) idxs refine)))

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
  
  (let: loop ([i : Natural  0]
              [ks : (Listof Value)   empty]
              [ws : (Listof Flonum)  empty]
              [t : Omega-Search-Tree  (build-search-tree Ω Z idxs refine)]
              [q : Flonum  1.0])
    (cond
      [(and (i . < . n) (q . > . 0.0) (not (failure-leaf? t)))
       (let ([i  (+ i 1)])
         (when (= 0 (remainder i 100))
           (printf "i = ~v~n" i)
           (flush-output))
         (let-values ([(s p t q)  (sample-search-tree t q)])
           (match s
             [(success-leaf (cons Ω Z) m)
              (match (refinement-sample-point Ω Z 1.0 idxs refine)
                [(omega-sample ω z m)
                 (define k (with-handlers ([forward-fail?  (λ: ([e : forward-fail]) e)])
                             (f-fwd ω z null)))
                 (cond [(and (not (forward-fail? k)) (set-member? K k))
                        (loop i (cons k ks) (cons (/ m p) ws) t q)]
                       [else
                        (loop i ks ws t q)])]
                [_
                 (loop i ks ws t q)])]
             [_
              (loop i ks ws t q)])))]
      [else
       (values ks ws)])))
