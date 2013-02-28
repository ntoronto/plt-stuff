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

(provide Refiner preimage-refiner
         (struct-out weighted-sample)
         print-sampler-stats
         interval-max-splits interval-min-length
         Omega-Sample refinement-sample drbayes-sample)

(: interval-max-splits (Parameterof Natural))
(define interval-max-splits (make-parameter 5))

(: interval-min-length (Parameterof Nonnegative-Flonum))
(define interval-min-length (make-parameter 1e-14))

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
;; Helper functions

(: take-index (All (A) ((Listof A) Integer -> (Values A (Listof A)))))
(define (take-index lst i)
  (cond [(index? i)
         (let: loop ([i : Index  i] [lst lst] [fst : (Listof A)  empty])
           (cond [(empty? lst)  (raise-argument-error 'take-index "nonempty List" 0 lst i)]
                 [(zero? i)  (values (first lst) (append (reverse fst) (rest lst)))]
                 [else  (loop (- i 1) (rest lst) (cons (first lst) fst))]))]
        [else
         (raise-argument-error 'take-index "Index" 1 lst i)]))

;; ===================================================================================================

(struct: (T) weighted-sample ([value : T] [weight : Flonum]) #:transparent)

(struct: fail-info ([idxs : Indexes]
                    [m : Natural]
                    [Ω : (U Empty-Set Omega-Rect)]
                    [Z : Branches-Rect])
  #:transparent)

(define splits 0)
(define branches 0)
(define backtracks 0)
(define (print-sampler-stats)
  (printf "splits = ~a~nbranches = ~a~nbacktracks = ~a~n"
          splits branches backtracks))

(: index-dist ((Listof Real) -> (Discrete-Dist Index)))
(define (index-dist ps)
  (discrete-dist (build-list (length ps) (λ: ([i : Index]) i)) ps))

(define-type Omega-Sample (weighted-sample (Pair Omega-Rect Branches-Rect)))

(: refinement-sample (Omega-Rect Branches-Rect Indexes Refiner -> Omega-Sample))
(define (refinement-sample orig-Ω orig-Z idxs refine)
  (define max-splits (interval-max-splits))
  (define min-ivl (interval-min-length))
  (let: loop ([idxs : Indexes  idxs]
              [m : Natural  0]
              [Ω : (U Empty-Set Omega-Rect)  orig-Ω]
              [Z : Branches-Rect  orig-Z]
              [prob : Flonum  1.0]
              [fail-queue : (Listof (Promise fail-info))  empty]
              [prob-queue : (Listof Flonum)  empty])
    (define (backtrack)
      (cond
        ;; Pick from the fail stack
        [(not (empty? fail-queue))
         (set! backtracks (+ 1 backtracks))
         (define i (sample (index-dist prob-queue)))
         (let-values ([(f fail-queue)  (take-index fail-queue i)]
                      [(p prob-queue)  (take-index prob-queue i)])
           (match-let ([(fail-info idxs m Ω Z)  (force f)])
             (loop idxs m Ω Z p fail-queue prob-queue)))]
        ;; Empty preimage, no recourses: total fail!
        [else
         (error 'drbayes-sample "cannot sample from the empty set")]))
    
    (cond
      ;; Empty preimage: failure!
      [(or (empty-set? Ω) (empty-set? Z))  (backtrack)]
      ;; Empty indexes: success!
      [(empty? idxs)  (weighted-sample (cons Ω Z) prob)]
      [else
       (define idx (let ([idx  (first idxs)])
                     (cond [(rational? idx)  (cons idx interval-split)]
                           [else  idx])))
       (match idx
         [(cons idx split)
          (cond
            [(m . < . max-splits)
             (define I (omega-rect-ref Ω idx))
             (define-values (Is ls) (split I min-ivl))
             (cond
               [(empty? Is)  (backtrack)]
               [(empty? (rest Is))
                (define new-I (first Is))
                (cond [(eq? I new-I)  (loop (rest idxs) 0 Ω Z prob fail-queue prob-queue)]
                      [else
                       (let-values ([(Ω Z)  (refine (omega-rect-set Ω idx new-I) Z)])
                         (loop idxs (+ m 1) Ω Z prob fail-queue prob-queue))])]
               [else
                (set! splits (+ 1 splits))
                
                (define total-l (flsum ls))
                (define ps (map (λ: ([l : Flonum]) (/ l total-l)) ls))
                (define i (sample (index-dist ps)))
                (let-values ([(I Is)  (take-index Is i)]
                             [(p ps)  (take-index ps i)])
                  (define new-fails
                    (map (λ: ([I : Interval])
                           (delay
                             (let-values ([(Ω Z)  (refine (omega-rect-set Ω idx I) Z)])
                               (fail-info idxs (+ m 1) Ω Z))))
                         Is))
                  (define new-probs (map (λ: ([p : Flonum]) (* prob p)) ps))
                  
                  (let-values ([(Ω Z)  (refine (omega-rect-set Ω idx I) Z)])
                    (loop idxs (+ m 1) Ω Z (* prob p)
                          (append new-fails fail-queue)
                          (append new-probs prob-queue))))])]
               [else
                (loop (rest idxs) 0 Ω Z prob fail-queue prob-queue)])]
         [(if-indexes r t-idxs f-idxs)
          
          (: choose-branch ((U 't 'f) (Promise Indexes)
                                      -> (Values Indexes
                                                 (U Empty-Set Omega-Rect)
                                                 Branches-Rect)))
          (define (choose-branch b b-idxs)
            (let-values ([(Ω Z)  (refine Ω (branches-rect-set Z r b))])
              (values (append (force b-idxs) (rest idxs)) Ω Z)))
          
          (define b (branches-rect-ref Z r))
          (case b
            [(t)  (let-values ([(idxs Ω Z)  (choose-branch 't t-idxs)])
                    (loop idxs 0 Ω Z prob fail-queue prob-queue))]
            [(f)  (let-values ([(idxs Ω Z)  (choose-branch 'f f-idxs)])
                    (loop idxs 0 Ω Z prob fail-queue prob-queue))]
            [else
             (set! branches (+ 1 branches))
             
             (define (choice1) (choose-branch 't t-idxs))
             (define (choice2) (choose-branch 'f f-idxs))
             
             (define p 0.5)
             (define 1-p (- 1.0 p))
             (let-values ([(choice1 choice2 p 1-p)  (if (< (random) p)
                                                        (values choice1 choice2 p 1-p)
                                                        (values choice2 choice1 1-p p))])
               (define f (delay (let-values ([(idxs Ω Z)  (choice2)])
                                  (fail-info idxs 0 Ω Z))))
               (let-values ([(idxs Ω Z)  (choice1)])
                 (loop idxs 0 Ω Z (* prob p)
                       (cons f fail-queue)
                       (cons (* prob 1-p) prob-queue))))])])])))

;; ===================================================================================================
;; Front end to sampler

(: drbayes-sample (case-> (expression Integer -> (Values (Listof Value) (Listof Flonum)))
                          (expression Integer Rect -> (Values (Listof Value) (Listof Flonum)))))
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
  (define omega-samples
    (let: loop ([ss : (Listof Omega-Sample)  empty] [i 0])
      (when (= 0 (remainder (+ i 1) 100))
        (printf "i = ~v~n" (+ i 1))
        (flush-output))
      (cond [(i . < . n)  (define s (refinement-sample Ω Z idxs refine))
                          (loop (cons s ss) (+ i 1))]
            [else  ss])))
  
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
