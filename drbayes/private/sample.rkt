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

;; Refinement control

(: interval-max-splits (Parameterof Natural))
(define interval-max-splits (make-parameter 5))

(: interval-min-length (Parameterof Nonnegative-Flonum))
(define interval-min-length (make-parameter 1e-14))

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

(: indexes->search-indexes (Indexes -> Indexes))
(define (indexes->search-indexes idxs)
  (define m (interval-max-splits))
  (let loop ([idxs idxs])
    (cond [(empty? idxs)  empty]
          [else
           (let-values ([(idx idxs)  (values (first idxs) (rest idxs))])
             (define new-idx
               (match idx
                 [(interval-index idx split)  (cons (interval-index idx split) m)]
                 [(if-indexes idx t-idxs f-idxs)
                  (if-indexes idx (delay (loop (force t-idxs))) (delay (loop (force f-idxs))))]
                 [_  idx]))
             (cons new-idx (loop idxs)))])))

(: build-search-tree (Omega-Rect Branches-Rect Indexes Refiner -> Omega-Search-Tree))
(define (build-search-tree Ω Z idxs refine)
  (define min-length (interval-min-length))
  (let: loop ([Ω : (U Empty-Set Omega-Rect)  Ω] [Z Z] [idxs  (indexes->search-indexes idxs)])
    (cond
      [(empty-set? Ω)  (failure-leaf)]
      [else
       (match idxs
         [(cons (cons (and iidx (interval-index idx split)) m) idxs)
          (cond
            [(zero? m)  (loop Ω Z idxs)]
            [else
             (define I (omega-rect-ref Ω idx))
             (define-values (Is ls) (split I min-length))
             (cond
               [(or (empty? Is) (empty? ls))
                (loop Ω Z idxs)]
               [(or (empty? (rest Is)) (empty? (rest ls)))
                (let-values ([(Ω Z)  (refine (omega-rect-set Ω idx (first Is)) Z)])
                  (loop Ω Z idxs))]
               [else
                (: make-node (Interval -> (Promise Omega-Search-Tree)))
                (define (make-node I)
                  (delay (let-values ([(Ω Z)  (refine (omega-rect-set Ω idx I) Z)])
                           (loop Ω Z (cons (cons iidx (- m 1)) idxs)))))
                
                (search-node (map/+2 make-node Is) (normalize-probs/+2 ls) 'splits)])])]
         [(cons (if-indexes idx t-idxs f-idxs) idxs)
          (: make-node ((U 't 'f) (Promise Indexes) -> Omega-Search-Tree))
          (define (make-node b b-idxs)
            (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z idx b))])
              (loop Ω Z (append (force b-idxs) idxs))))
          
          (define b (branches-rect-ref Z idx))
          (case b
            [(t)  (make-node 't t-idxs)]
            [(f)  (make-node 'f f-idxs)]
            [else  (search-node (list (delay (make-node 't t-idxs)) (delay (make-node 'f f-idxs)))
                                (list 0.5 0.5)
                                'branches)])]
         [(cons idx idxs)
          (error 'build-search-tree "internal error: did not expect index ~e" idx)]
         [(list)
          (success-leaf (cons Ω Z))])])))


(: refinement-sample* (Omega-Rect Branches-Rect Indexes Refiner Natural -> (Listof Omega-Sample)))
(define (refinement-sample* Ω Z idxs refine n)
  (define t (build-search-tree Ω Z idxs refine))
  (define-values (ts ps) (sample-search-tree* t n))
  (map (λ: ([t : (success-leaf Omega-Pair)] [p : Flonum])
         (weighted-sample (success-leaf-value t) p))
       ts ps))

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
