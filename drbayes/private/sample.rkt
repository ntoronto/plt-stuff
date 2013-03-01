#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         math/flonum
         math/distributions
         "omega.rkt"
         "rect.rkt"
         "indexes.rkt"
         "arrow.rkt")

(provide (all-defined-out))

(: interval-max-splits (Parameterof Natural))
(define interval-max-splits (make-parameter 5))

(: interval-min-length (Parameterof Nonnegative-Flonum))
(define interval-min-length (make-parameter 1e-14))

(define splits 0)
(define branches 0)
(define backtracks 0)
(define (print-sampler-stats)
  (printf "splits = ~a~nbranches = ~a~nbacktracks = ~a~n"
          splits branches backtracks))

;; ===================================================================================================

(define-type Refiner
  ((U Empty-Set Omega-Rect) Branches-Rect -> (Values (U Empty-Set Omega-Rect) Branches-Rect)))

(: preimage-refiner (Computation Nonempty-Rect -> Refiner))
(define ((preimage-refiner e-comp B) Ω Z)
  (match-let ([(computation-meaning Z _ e-pre)  (e-comp Ω null-rect Z)])
    (let-values ([(Ω Γ)  (e-pre B)])
      (cond [(not (or (empty-set? Γ) (null-rect? Γ)))
             (raise-result-error 'preimage-refiner "(U Empty-Set Null-Rect)" Γ)]
            [else  (values Ω Z)]))))

;; ===================================================================================================
;; Helpers

(define-type (Listof+2 A) (Pair A (Pair A (Listof A))))

(: map/+2 (All (A B) ((A -> B) (Listof+2 A) -> (Listof+2 B))))
(define (map/+2 f xs)
  (list* (f (first xs))
         (f (second xs))
         (map f (rest (rest xs)))))

(: normalize-probs/+2 ((Listof+2 Flonum) -> (Listof+2 Flonum)))
(define (normalize-probs/+2 qs)
  (define p (flsum qs))
  (map/+2 (λ: ([q : Flonum]) (/ q p)) qs))

(: take-index (All (A) ((Listof A) Integer -> (Values A (Listof A)))))
(define (take-index lst i)
  (cond [(index? i)
         (let: loop ([i : Index  i] [lst lst] [fst : (Listof A)  empty])
           (cond [(empty? lst)  (raise-argument-error 'take-index "nonempty List" 0 lst i)]
                 [(zero? i)  (values (first lst) (append (reverse fst) (rest lst)))]
                 [else  (loop (- i 1) (rest lst) (cons (first lst) fst))]))]
        [else
         (raise-argument-error 'take-index "Index" 1 lst i)]))

(: take-index/+2 (All (A) ((Listof+2 A) Integer -> (Values A (Pair A (Listof A))))))
(define (take-index/+2 lst i)
  (cond [(= i 0)  (values (first lst) (cdr lst))]
        [(= i 1)  (values (second lst) (cons (first lst) (rest (rest lst))))]
        [else
         (let-values ([(x rest-lst)  (take-index (rest (rest lst)) (- i 2))])
           (values x (list* (first lst) (second lst) rest-lst)))]))

(define-syntax-rule (maybe-force p-expr)
  (let ([p p-expr])
    (if (promise? p) (force p) p)))

(: indexes->search-indexes (Indexes -> Indexes))
(define (indexes->search-indexes idxs)
  (define m (interval-max-splits))
  (let loop ([idxs idxs])
    (cond [(empty? idxs)  empty]
          [else
           (let-values ([(idx idxs)  (values (first idxs) (rest idxs))])
             (define new-idx
               (match idx
                 [(? rational? idx)  (cons (interval-index idx interval-split) m)]
                 [(interval-index idx split)  (cons (interval-index idx split) m)]
                 [(if-indexes idx t-idxs f-idxs)
                  (if-indexes idx (delay (loop (force t-idxs))) (delay (loop (force f-idxs))))]
                 [_  idx]))
             (cons new-idx (loop idxs)))])))

(: index-dist ((Listof Real) -> (Discrete-Dist Index)))
(define (index-dist ps)
  (discrete-dist (build-list (length ps) (λ: ([i : Index]) i)) ps))

;; ===================================================================================================

(struct: (T) weighted-sample ([value : T] [weight : Flonum]) #:transparent)

(define-type Omega-Pair (Pair Omega-Rect Branches-Rect))
(define-type Omega-Sample (weighted-sample Omega-Pair))

(define-type Search-Tree (U Search-Leaf search-node))
(define-type Search-Leaf (U #f Omega-Pair))
(struct: search-node ([trees : (Listof+2 (U Search-Tree (Promise Search-Tree)))]
                      [probs : (Listof+2 Flonum)]
                      [split? : Boolean])
  #:transparent)

(: build-search-tree (Omega-Rect Branches-Rect Indexes Refiner -> Search-Tree))
(define (build-search-tree Ω Z idxs refine)
  (define min-length (interval-min-length))
  (let: loop ([Ω : (U Empty-Set Omega-Rect)  Ω] [Z Z] [idxs  (indexes->search-indexes idxs)])
    (cond
      [(empty-set? Ω)  #f]
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
                (: make-node (Interval -> (Promise Search-Tree)))
                (define (make-node I)
                  (delay (let-values ([(Ω Z)  (refine (omega-rect-set Ω idx I) Z)])
                           (loop Ω Z (cons (cons iidx (- m 1)) idxs)))))
                
                (search-node (map/+2 make-node Is) (normalize-probs/+2 ls) #t)])])]
         [(cons (if-indexes idx t-idxs f-idxs) idxs)
          (: make-node ((U 't 'f) (Promise Indexes) -> Search-Tree))
          (define (make-node b b-idxs)
            (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z idx b))])
              (loop Ω Z (append (force b-idxs) idxs))))
          
          (define b (branches-rect-ref Z idx))
          (case b
            [(t)  (make-node 't t-idxs)]
            [(f)  (make-node 'f f-idxs)]
            [else  (search-node (list (delay (make-node 't t-idxs)) (delay (make-node 'f f-idxs)))
                                (list 0.5 0.5)
                                #f)])]
         [(cons idx idxs)
          (error 'impossible)]
         [(list)
          (cons Ω Z)])])))

(: sample-search-tree (Search-Tree -> (U #f Omega-Sample)))
(define (sample-search-tree t)
  (let loop ([t t] [p 1.0])
    (match t
      [(search-node ts qs split?)
       (cond [split?  (set! splits (+ 1 splits))]
             [else    (set! branches (+ 1 branches))])
       (define i (sample (index-dist qs)))
       (let ([t  (list-ref ts i)]
             [q  (list-ref qs i)])
         (loop (if (promise? t) (force t) t) (* p q)))]
      [(? pair? t)
       (weighted-sample t p)]
      [_  #f])))

(: refinement-sample (Omega-Rect Branches-Rect Indexes Refiner -> (U #f Omega-Sample)))
(define (refinement-sample Ω Z idxs refine)
  (sample-search-tree (build-search-tree Ω Z idxs refine)))

#|
(: refinement-sample* (Omega-Rect Branches-Rect Indexes Refiner Natural -> (Listof Omega-Sample)))
;; Stupid rejection sampler (no backtracking)
(define (refinement-sample* Ω Z idxs refine n)
  (let: loop ([i 0] [ss : (Listof Omega-Sample)  empty])
    (cond [(i . < . n)
           (when (= 0 (remainder (+ i 1) 100))
             (printf "i = ~v~n" (+ i 1))
             (flush-output))
           (define s (refinement-sample Ω Z idxs refine))
           (cond [s     (loop (+ i 1) (cons s ss))]
                 [else  (loop i ss)])]
          [else  ss])))
|#

(: sample-search-tree* (Search-Tree -> (Values (U #f Omega-Pair) Flonum Search-Tree)))
(define (sample-search-tree* t)
  (match t
    [(search-node cs qs split?)
     (cond [split?  (set! splits (+ 1 splits))]
           [else    (set! branches (+ 1 branches))])
     (define i (sample (index-dist qs)))
     (let*-values ([(c cs)  (take-index/+2 cs i)]
                   [(q qs)  (take-index/+2 qs i)]
                   [(s p c)  (sample-search-tree* (maybe-force c))]
                   [(p)  (* p q)])
       (define new-t
         (cond [s  t]
               [(q . > . p)
                (search-node (cons c cs)
                             (normalize-probs/+2 (cons (- q p) qs))
                             split?)]
               [(or (empty? (rest cs)) (empty? (rest qs)))
                (maybe-force (first cs))]
               [else
                (search-node cs (normalize-probs/+2 qs) split?)]))
       (values s p new-t))]
    [(? pair? t)
     (values t 1.0 t)]
    [_
     (values #f 1.0 #f)]))

(: refinement-sample* (Omega-Rect Branches-Rect Indexes Refiner Natural -> (Listof Omega-Sample)))
(define (refinement-sample* Ω Z idxs refine n)
  (let: loop ([i  0]
              [ss : (Listof Omega-Sample)  empty]
              [t  (build-search-tree Ω Z idxs refine)]
              [q  1.0])
    (cond [(i . < . n)
           (when (= 0 (remainder (+ i 1) 100))
             (printf "i = ~v~n" (+ i 1))
             (flush-output))
           (let-values ([(s p t)  (sample-search-tree* t)])
             (cond [s     (loop (+ i 1) (cons (weighted-sample s (* p q)) ss) t q)]
                   [else  (set! backtracks (+ 1 backtracks))
                          (loop i ss t (- q (* p q)))]))]
          [else  ss])))

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
