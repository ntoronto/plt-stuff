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

(struct: omega-sample ([Ω : Omega-Rect] [Z : Branches-Rect] [measure : Flonum] [prob : Flonum])
  #:transparent)

;; ===================================================================================================

(define-type Refiner (Omega-Rect Branches-Rect -> (Values Maybe-Omega-Rect Maybe-Branches-Rect)))

(: preimage-refiner (Computation Nonempty-Set -> Refiner))
(define ((preimage-refiner e-comp K) Ω Z)
  (match-let ([(computation-meaning Ze Ke e-pre)  (e-comp Ω Z null-rect)])
    (let-values ([(Ω Z Γ)  (e-pre (branches-rect-intersect Z Ze)
                                  (set-intersect K Ke))])
      (cond [(not (or (empty-set? Γ) (null-rect? Γ)))
             (raise-result-error 'preimage-refiner "(U Empty-Set Null-Rect)" Γ)]
            [else
             (values Ω Z)]))))

(: refinement-sample* (Omega-Rect Branches-Rect Indexes Refiner Natural -> (Listof omega-sample)))
(define (refinement-sample* Ω Z idxs refine n)
  (define t (build-search-tree Ω Z idxs refine))
  (define-values (ts ps) (sample-search-tree* t n))
  (map (λ: ([t : (success-leaf Omega-Pair)] [p : Flonum])
         (match-define (success-leaf (cons Ω Z) m) t)
         (omega-sample Ω Z m p))
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
;; Front end to sampler

(: drbayes-sample (case-> (expression Natural -> (Values (Listof Value) (Listof Flonum)))
                          (expression Natural Set -> (Values (Listof Value) (Listof Flonum)))))
(define (drbayes-sample f n [K universe])
  (match-define (expression-meaning idxs f-fwd f-comp) (run-expression f))
  
  (define (empty-set-error)
    (error 'drbayes-sample "cannot sample from the empty set"))
  
  (define refine
    (cond [(empty-set? K)  (empty-set-error)]
          [else  (preimage-refiner f-comp K)]))
  
  (define-values (Ω Z)
    (let-values ([(Ω Z)  (refine omega-rect branches-rect)])
      (if (or (empty-set? Ω) (empty-set? Z)) (empty-set-error) (values Ω Z))))
  
  (: omega-samples (Listof omega-sample))
  (define omega-samples (refinement-sample* Ω Z idxs refine n))
  
  (let: loop ([omega-samples  omega-samples]
              [ks : (Listof Value)  empty]
              [ws : (Listof Flonum)  empty])
    (cond [(empty? omega-samples)  (values ks ws)]
          [else
           (match-define (omega-sample Ω Z m p) (first omega-samples))
           (define ω (omega-rect-sample-point Ω))
           (define z (branches-rect-sample-point Z))
           (define k (with-handlers ([if-bad-branch?  (λ (exn) (assert exn if-bad-branch?))])
                       (f-fwd ω z null)))
           (cond [(and (not (if-bad-branch? k)) (set-member? K k))
                  (loop (rest omega-samples)
                        (cons k ks)
                        (cons (/ m p) ws))]
                 [else
                  (loop (rest omega-samples) ks ws)])])))
