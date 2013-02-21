#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         math/distributions
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
  ((U Empty-Set Omega-Rect) Branches -> (Values (U Empty-Set Omega-Rect) Branches)))

(: preimage-refiner (Computation Nonempty-Rect -> Refiner))
(define ((preimage-refiner e B) A bs)
  (let* ([f  (e A bs)]
         [new-bs  (function-new-branches f)]
         [A  (apply-preimage f B)])
    (cond [(or (empty-set? A) (omega-rect? A))  (values A (add-new-branches bs new-bs))]
          [else  (raise-result-error 'preimage-refiner "(U Empty-Set Omega-Rect)" A)])))

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
                    [branches : Branches])
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

(define-type Omega-Sample (weighted-sample (Pair Omega-Rect Branches)))

(: num-omega-rect-changes (Omega-Rect Omega-Rect -> Integer))
(define (num-omega-rect-changes Ω1 Ω2)
  (define h1 (omega-rect-hash Ω1))
  (define h2 (omega-rect-hash Ω2))
  (define ks (list-union (hash-keys h1) (hash-keys h2)))
  (let loop ([ks ks] [sum 0])
    (cond [(empty? ks)  sum]
          [else
           (define k (first ks))
           (if (equal? (omega-rect-ref Ω1 k) (omega-rect-ref Ω2 k))
               (loop (rest ks) sum)
               (loop (rest ks) (+ 1 sum)))])))

(: num-branch-changes (Branches Branches -> Integer))
(define (num-branch-changes bs1 bs2)
  (define ks (list-union (hash-keys bs1) (hash-keys bs2)))
  (let loop ([ks ks] [sum 0])
    (cond [(empty? ks)  sum]
          [else
           (define k (first ks))
           (if (equal? (branches-ref bs1 k) (branches-ref bs2 k))
               (loop (rest ks) sum)
               (loop (rest ks) (+ 1 sum)))])))

(: refinement-sample (Omega-Rect Branches Indexes Refiner -> Omega-Sample))
(define (refinement-sample orig-Ω orig-bs idxs refine)
  (define max-splits (interval-max-splits))
  (define min-ivl (interval-min-length))
  (let: loop ([idxs : Indexes  idxs]
              [m : Natural  0]
              [Ω : (U Empty-Set Omega-Rect)  orig-Ω]
              [bs : Branches  orig-bs]
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
    
    #;
    (unless (empty-set? Ω)
      (printf "number of Ω changes: ~v~n" (+ (num-omega-rect-changes orig-Ω Ω)
                                             (num-branch-changes orig-bs bs)))
      (when (and (eq? orig-Ω Ω) (eq? orig-bs bs))
        (printf "**** equivalent ****~n"))
      (set! orig-Ω Ω)
      (set! orig-bs bs))
    
    (cond
      ;; Empty preimage: failure!
      [(empty-set? Ω)  (backtrack)]
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
                      [else  (let-values ([(Ω bs)  (refine (omega-rect-set Ω idx new-I) bs)])
                               (loop idxs (+ m 1) Ω bs prob fail-queue prob-queue))])]
               [else
                (set! splits (+ 1 splits))
                
                (define d (index-dist ps))
                (define i (sample d))
                (let-values ([(I Is)  (take-index Is i)]
                             [(p ps)  (take-index (discrete-dist-probs d) i)])
                  (define new-fails
                    (map (λ: ([I : Interval])
                           (delay (let-values ([(Ω bs)  (refine (omega-rect-set Ω idx I) bs)])
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
                                      -> (Values Indexes (U Empty-Set Omega-Rect) Branches)))
          (define (choose-branch b b-idxs)
            (let*-values ([(bs)    (branches-set bs r b)]
                          [(Ω bs)  (refine Ω bs)])
              (values (append (force b-idxs) (rest idxs)) Ω bs)))
          
          (define b (branches-ref bs r))
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
  (match-define (meaning idxs f-fwd f-comp) (run-expression (program/exp f)))
  
  (define (empty-set-fail)
    (error 'drbayes-sample "cannot sample from the empty set"))
  
  (define refine
    (cond [(empty-set? B)  (empty-set-fail)]
          [else  (preimage-refiner f-comp B)]))
  
  (define-values (Ω bs)
    (let-values ([(Ω bs)  (refine (omega-rect) empty-branches)])
      (if (empty-set? Ω) (empty-set-fail) (values Ω bs))))
  
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
               (define x (f-fwd ω bs))
               (cond [(rect-member? B x)
                      (define m (omega-rect-measure Ω))
                      (cons (weighted-sample x (/ m p)) (loop (rest omega-samples)))]
                     [else
                      (loop (rest omega-samples))]))])))
  
  (values (map (inst weighted-sample-value Value) image-samples)
          (map (inst weighted-sample-weight Value) image-samples)))
