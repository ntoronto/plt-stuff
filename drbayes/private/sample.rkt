#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         math/distributions
         "types.rkt"
         "rect.rkt"
         "arrow.rkt"
         "expression.rkt")

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
  ((U Empty-Set Omega-Rect)
   (U Empty-Set Branches-Rect)
   -> (Values (U Empty-Set Omega-Rect)
              (U Empty-Set Branches-Rect))))

(: preimage-refiner (Computation Nonempty-Rect -> Refiner))
(define ((preimage-refiner e-comp B) Ω bs)
  (match-let ([(computation-meaning bs range e-pre)  (e-comp Ω null-rect bs)])
    (let-values ([(Ω Γ)  (e-pre B)])
      (cond [(not (or (empty-set? Γ) (null-rect? Γ)))
             (raise-result-error 'preimage-refiner "(U Empty-Set Null-Rect)" Γ)]
            [else  (values Ω bs)]))))

;; ===================================================================================================
;; Helper functions

(: take-index (All (A) ((Listof A) Integer -> (Values A (Listof A)))))
(define (take-index lst i)
  (cond [(index? i)
         (let: loop ([i : Index  i] [lst lst] [fst : (Listof A)  empty])
           (cond [(empty? lst)  (raise-argument-error 'take-index "nonempty List" 0 list i)]
                 [(zero? i)  (values (first lst) (append (reverse fst) (rest lst)))]
                 [else  (loop (- i 1) (rest lst) (cons (first lst) fst))]))]
        [else
         (raise-argument-error 'take-index "Index" 1 lst i)]))

;; ===================================================================================================

(struct: (T) weighted-sample ([value : T] [weight : Flonum]) #:transparent)

(struct: fail-info ([idxs : Indexes]
                    [m : Natural]
                    [Ω : (U Empty-Set Omega-Rect)]
                    [branches : (U Empty-Set Branches-Rect)])
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
(define (refinement-sample orig-Ω orig-bs idxs refine)
  (define max-splits (interval-max-splits))
  (define min-ivl (interval-min-length))
  (let: loop ([idxs : Indexes  idxs]
              [m : Natural  0]
              [Ω : (U Empty-Set Omega-Rect)  orig-Ω]
              [bs : (U Empty-Set Branches-Rect)  orig-bs]
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
           (match-let ([(fail-info idxs m Ω bs)  (force f)])
             (loop idxs m Ω bs p fail-queue prob-queue)))]
        ;; Empty preimage, no recourses: total fail!
        [else
         (error 'drbayes-sample "cannot sample from the empty set")]))
    
    (cond
      ;; Empty preimage: failure!
      [(or (empty-set? Ω) (empty-set? bs))  (backtrack)]
      ;; Empty indexes: success!
      [(empty? idxs)  (weighted-sample (cons Ω bs) prob)]
      [else
       (define idx (let ([idx  (first idxs)])
                     (cond [(rational? idx)  (cons idx interval-split)]
                           [else  idx])))
       (match idx
         [(cons idx split)
          (cond
            [(m . < . max-splits)
             (define I (omega-rect-ref Ω idx))
             (define-values (Is ps) (split I min-ivl))
             (cond
               [(empty? Is)  (backtrack)]
               [(empty? (rest Is))
                (define new-I (first Is))
                (cond [(eq? I new-I)  (loop (rest idxs) 0 Ω bs prob fail-queue prob-queue)]
                      [else
                       (let-values ([(Ω bs)  (refine (omega-rect-set Ω idx new-I) bs)])
                         (loop idxs (+ m 1) Ω bs prob fail-queue prob-queue))])]
               [else
                (set! splits (+ 1 splits))
                
                (define d (index-dist ps))
                (define i (sample d))
                (let-values ([(I Is)  (take-index Is i)]
                             [(p ps)  (take-index (discrete-dist-probs d) i)])
                  (define new-fails
                    (map (λ: ([I : Interval])
                           (delay
                             (let-values ([(Ω bs)  (refine (omega-rect-set Ω idx I) bs)])
                               (fail-info idxs (+ m 1) Ω bs))))
                         Is))
                  (define new-probs (map (λ: ([p : Flonum]) (* prob p)) ps))
                  
                  (let-values ([(Ω bs)  (refine (omega-rect-set Ω idx I) bs)])
                    (loop idxs (+ m 1) Ω bs (* prob p)
                          (append new-fails fail-queue)
                          (append new-probs prob-queue))))])]
               [else
                (loop (rest idxs) 0 Ω bs prob fail-queue prob-queue)])]
         [(if-indexes r t-idxs f-idxs)
          
          (: choose-branch ((U 't 'f) (Promise Indexes)
                                      -> (Values Indexes
                                                 (U Empty-Set Omega-Rect)
                                                 (U Empty-Set Branches-Rect))))
          (define (choose-branch b b-idxs)
            (let-values ([(Ω bs)  (refine Ω (branches-rect-restrict bs r b))])
              (values (append (force b-idxs) (rest idxs)) Ω bs)))
          
          (define b (branches-rect-ref bs r))
          (case b
            [(t)  (let-values ([(idxs Ω bs)  (choose-branch 't t-idxs)])
                    (loop idxs 0 Ω bs prob fail-queue prob-queue))]
            [(f)  (let-values ([(idxs Ω bs)  (choose-branch 'f f-idxs)])
                    (loop idxs 0 Ω bs prob fail-queue prob-queue))]
            [else
             (set! branches (+ 1 branches))
             
             (define (choice1) (choose-branch 't t-idxs))
             (define (choice2) (choose-branch 'f f-idxs))
             
             (define p 0.5)
             (define 1-p (- 1.0 p))
             (let-values ([(choice1 choice2 p 1-p)  (if (< (random) p)
                                                        (values choice1 choice2 p 1-p)
                                                        (values choice2 choice1 1-p p))])
               (define f (delay (let-values ([(idxs Ω bs)  (choice2)])
                                  (fail-info idxs 0 Ω bs))))
               (let-values ([(idxs Ω bs)  (choice1)])
                 (loop idxs 0 Ω bs (* prob p)
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
  
  (define-values (Ω bs)
    (let-values ([(Ω bs)  (refine (omega-rect) branches-rect)])
      (if (or (empty-set? Ω) (empty-set? bs)) (empty-set-error) (values Ω bs))))
  
  (: omega-samples (Listof Omega-Sample))
  (define omega-samples
    (let: loop ([ss : (Listof Omega-Sample)  empty] [i 0])
      (when (= 0 (remainder (+ i 1) 100))
        (printf "i = ~v~n" (+ i 1))
        (flush-output))
      (cond [(i . < . n)  (define s (refinement-sample Ω bs idxs refine))
                          (loop (cons s ss) (+ i 1))]
            [else  ss])))
  
  (: image-samples (Listof (weighted-sample Value)))
  (define image-samples
    (let: loop ([omega-samples  omega-samples])
      (cond [(empty? omega-samples)  empty]
            [else
             (match-define (weighted-sample (cons Ω bs) p) (first omega-samples))
             (define ω (omega-rect-sample-point Ω))
             (with-handlers ([if-bad-branch?  (λ (_) (loop (rest omega-samples)))])
               (define x (f-fwd ω null bs))
               (cond [(rect-member? B x)
                      (define m (omega-rect-measure Ω))
                      (cons (weighted-sample x (/ m p)) (loop (rest omega-samples)))]
                     [else
                      (loop (rest omega-samples))]))])))
  
  (values (map (inst weighted-sample-value Value) image-samples)
          (map (inst weighted-sample-weight Value) image-samples)))
