#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         math/flonum
         math/distributions
         "omega.rkt"
         "rect.rkt"
         "indexes.rkt"
         "arrow.rkt"
         "search.rkt"
         "utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(define-type Omega-Pair (Pair Omega-Rect Branches-Rect))
(define-type Omega-Search-Tree (Search-Tree Omega-Pair))

(struct: (T) weighted-sample ([value : T] [weight : Flonum]) #:transparent)

(define-type Omega-Sample (weighted-sample Omega-Pair))

;; ===================================================================================================

(define-type Refiner
  ((U Empty-Set Omega-Rect) Branches-Rect -> (Values (U Empty-Set Omega-Rect) Branches-Rect)))

(: preimage-refiner (Computation Nonempty-Rect -> Refiner))
(define ((preimage-refiner e-comp B) Ω Z)
  (match-let ([(computation-meaning Z _ e-pre)  (e-comp Ω null-rect Z)])
    (let-values ([(Ω Γ)  (e-pre B)])
      (cond [(not (or (empty-set? Γ) (null-rect? Γ)))
             (raise-result-error 'preimage-refiner "(U Empty-Set Null-Rect)" Γ)]
            [else
             (values Ω Z)]))))

(: refinement-sample* (Omega-Rect Branches-Rect Indexes Refiner Natural -> (Listof Omega-Sample)))
(define (refinement-sample* Ω Z idxs refine n)
  (define t (build-search-tree Ω Z idxs refine))
  (define-values (ts ps) (sample-search-tree* t n))
  (map (λ: ([t : (success-leaf Omega-Pair)] [p : Flonum])
         (weighted-sample (success-leaf-value t) p))
       ts ps))

(: build-search-tree ((U Empty-Set Omega-Rect) Branches-Rect Indexes Refiner -> Omega-Search-Tree))
(define (build-search-tree Ω Z idxs refine)
  (cond
    [(empty-set? Ω)  (failure-leaf)]
    [(empty? idxs)  (success-leaf (cons Ω Z))]
    [(if-indexes? (first idxs))
     (build-search-tree/if Ω Z (first idxs) (rest idxs) refine)]
    [else  
     (build-search-tree/ivl Ω Z (first idxs) (rest idxs) refine)]))

(: build-search-tree/if (Omega-Rect Branches-Rect if-indexes Indexes Refiner
                                    -> Omega-Search-Tree))
(define (build-search-tree/if Ω Z idx idxs refine)
  (match-define (if-indexes i t-idxs f-idxs) idx)
  
  (: make-node ((U 't 'f) (Promise Indexes) -> Omega-Search-Tree))
  (define (make-node b b-idxs)
    (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z i b))])
      (build-search-tree Ω Z (append (force b-idxs) idxs) refine)))
  
  (define b (branches-rect-ref Z i))
  (case b
    [(t)  (make-node 't t-idxs)]
    [(f)  (make-node 'f f-idxs)]
    [else  (search-node (list (delay (make-node 't t-idxs)) (delay (make-node 'f f-idxs)))
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
                          (expression Natural Rect -> (Values (Listof Value) (Listof Flonum)))))
(define (drbayes-sample f n [B universal-set])
  (match-define (expression-meaning idxs f-fwd f-comp) (run-expression f))
  
  (define (empty-set-error)
    (error 'drbayes-sample "cannot sample from the empty set"))
  
  (define refine
    (cond [(empty-set? B)  (empty-set-error)]
          [else  (preimage-refiner f-comp B)]))
  
  (define-values (Ω Z)
    (let-values ([(Ω Z)  (refine omega-rect branches-rect)])
      (if (or (empty-set? Ω) (empty-set? Z)) (empty-set-error) (values Ω Z))))
  
  (: omega-samples (Listof Omega-Sample))
  (define omega-samples (refinement-sample* Ω Z idxs refine n))
  
  (: image-samples (Listof (weighted-sample Value)))
  (define image-samples
    (let: loop ([omega-samples  omega-samples])
      (cond [(empty? omega-samples)  empty]
            [else
             (match-define (weighted-sample (cons Ω Z) p) (first omega-samples))
             (define ω (omega-rect-sample-point Ω))
             (with-handlers ([if-bad-branch?  (λ (_) (loop (rest omega-samples)))])
               (define x (f-fwd ω null Z))
               (cond [(rect-member? B x)
                      (define m (omega-rect-measure Ω))
                      (cons (weighted-sample x (/ m p)) (loop (rest omega-samples)))]
                     [else
                      (loop (rest omega-samples))]))])))
  
  (values (map (inst weighted-sample-value Value) image-samples)
          (map (inst weighted-sample-weight Value) image-samples)))
