#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         "../main.rkt"
         "test-utils.rkt"
         "normal-normal.rkt")

(printf "starting...~n~n")

(interval-max-splits 5)
(define n 1000)

#;; Test: list/arr (really pair/arr and unit/arr) random/arr
;; Preimage is a rectangle [0.25,0.5] × [0.25,0.5] × [0.25,0.5]
(begin
  (define f-expr (list/arr random/arr random/arr random/arr))
  (define I (interval 0.25 0.5 #t #t))
  (define B (list-rect I I I)))

#;; Test: list/arr random/arr ap/arr sqr/arr
;; Preimage is a rectangle [0.25,0.5] × [0.5,sqrt(1/2)] × [0.5,sqrt(1/2)]
(begin
  (define f-expr
    (list/arr random/arr
              (ap/arr sqr/arr random/arr)
              (ap/arr sqr/arr random/arr)))
  (define I (interval 0.25 0.5 #t #t))
  (define B (list-rect I I I)))

#;; Test: list/arr random/arr ap/arr sqr/arr ref/arr
;; Preimage is the same as just above: [0.25,0.5] × [0.5,sqrt(1/2)] × [0.5,sqrt(1/2)]
(begin
  (define f-expr
    (ap/arr (list/arr (ref/arr 0)
                      (ref/arr 1)
                      (ref/arr 2))
            (list/arr random/arr
                      (ap/arr sqr/arr random/arr)
                      (ap/arr sqr/arr random/arr))))
  (define I (interval 0.25 0.5 #t #t))
  (define B (list-rect I I I)))

#;; Test: list/arr random/arr ap/arr ref/arr +/arr
;; Preimage is a 2D downard diagonal strip × [0,1]
(begin
  (define f-expr
    (ap/arr (list/arr (ref/arr 0)
                      (ref/arr 1)
                      (ap/arr +/arr (pair/arr (ref/arr 0) (ref/arr 1))))
            (list/arr random/arr
                      random/arr
                      random/arr)))
  (define B (list-rect reals reals (interval 0.45 0.7 #t #t))))

#;; Test: same as above, with expressions
(begin
  (interval-max-splits 2)
  
  (define f-expr
    (program/exp
     (drbayes
      (let ([x  (uniform)]
            [y  (uniform)])
        (list x y (+ x y))))))
  
  (define B (list-rect reals reals (interval 0.45 0.7))))

#;
(begin
  (interval-max-splits 4)
  
  (define f-expr
    (program/exp
     (drbayes
      (let ([x  (translate (scale (uniform) (const 2.0)) (const -1.0))]
            [y  (translate (scale (uniform) (const 2.0)) (const -1.0))])
        (list x y (/ x y))))))
  
  (define B (list-rect reals reals (interval -1.25 0.8))))

#;; Test: boolean #t, #f or both
;; Preimage should be:
;;    #t: [0,0.5)
;;    #f: (0.5,1]
;;  both: [0,1]
(begin
  (define f-expr
    (list/arr (ap/arr negative?/arr (ap/arr normal/arr random/arr))))
  ;(define B (list-rect 't))
  (define B (list-rect 'f))
  ;(define B (list-rect 'tf))
  )

#;; Test: less than
;; Preimage should be:
;;    #t: upper triangle (points above y = x)
;;    #f: lower triangle (points below y = x)
;;  both: [0,1] × [0,1]
(begin
  (define f-expr
    (list/arr (ap/arr lt/arr (pair/arr random/arr random/arr))))
  (define B (list-rect 't))
  ;(define B (list-rect 'f))
  ;(define B (list-rect 'tf))
  )

#;; Test: strict-or
;; Preimage should be the union of a large upper triangle and a small lower triangle, and
;; samples should be uniformly distributed
(begin
  (define f-expr
    (program/exp
     (drbayes
      (let ([x  (uniform)]
            [y  (uniform)])
        (list x y (or (< x y) (> x (scale y (const 8)))))))))
  (define B (list-rect reals reals 't)))

#;; Test: Normal-Normal model
;; Preimage should be a banana shape
(begin
  (define f-expr
    (ap/arr (let* ([X0  (ap/arr normal/arr (ref/arr 0))]
                   [X1  (ap/arr +/arr (pair/arr X0 (ap/arr normal/arr (ref/arr 1))))])
              (list/arr X0 X1))
            (list/arr random/arr
                      random/arr)))
  (define B (list-rect reals (interval 0.9 1.1 #t #t)))
  (normal-normal/lw 0 1 '(1.0) '(1.0)))

#;; Test: Normal-Normal model, using expressions
;; Preimage should be as above
(begin
  (define f-expr
    (program/exp
     (drbayes
      (let* ([x  (normal)]
             [y  (normal x)])
        (list x y)))))
  (define B (list-rect reals (interval 0.9 1.1 #t #t)))
  (normal-normal/lw 0 1 '(1.0) '(1.0)))

#;; Test: thermometer that goes to 100
(begin
  (define f-expr
    (program/exp
     (drbayes
      (let* ([x  (normal 90 10)]
             [y  (normal x 1)])
        (list x (strict-if (y . > . 100) 100 y))))))
  
  (define B (list-rect reals (interval 100.0 100.0))))

#;; Test: Normal-Normal model with circular condition
;; Preimage should look like a football set up for a field goal
(begin
  (define f-expr
    (ap/arr (let* ([X0  (ap/arr normal/arr (ref/arr 0))]
                   [X1  (ap/arr +/arr (pair/arr X0 (ap/arr normal/arr (ref/arr 1))))])
              (list/arr X0 X1 (ap/arr sqrt/arr (ap/arr +/arr (pair/arr (ap/arr sqr/arr X0)
                                                                       (ap/arr sqr/arr X1))))))
            (list/arr random/arr
                      random/arr)))
  (define B (list-rect reals reals (interval 0.95 1.05 #t #t))))

#;; Test: Normal-Normal model with circular condition, using expressions
;; Preimage should be as above
(begin
  (define f-expr
    (program/exp
     (drbayes
      (let* ([x0  (normal)]
             [x1  (normal x0)])
        (list x0 x1 (sqrt (+ (sqr x0) (sqr x1))))))))
  (define B (list-rect reals reals (interval 0.95 1.05 #t #t))))

#;; Test: Normal-Normal or Cauchy-Cauchy, depending on random variable
(begin
  (define f-expr
    (ap/arr (let* ([B  (ap/arr negative?/arr (ap/arr -/arr (pair/arr (ref/arr 0)
                                                                     (c/arr #i499/1000))))]
                   [X0  (strict-if/arr B
                                       (ap/arr normal/arr (ref/arr 1))
                                       (ap/arr cauchy/arr (ref/arr 1)))]
                   [X1  (strict-if/arr B
                                       (ap/arr +/arr (pair/arr X0 (ap/arr normal/arr (ref/arr 2))))
                                       (ap/arr +/arr (pair/arr X0 (ap/arr cauchy/arr (ref/arr 2)))))])
              (list/arr X0 X1 B))
            (list/arr random/arr random/arr random/arr)))
  (define B (list-rect reals (interval 0.9 1.1 #t #t) 'tf)))

#;; Test: Normal-Normal or Cauchy-Cauchy, depending on random variable
(begin
  (define f-expr
    (program/exp
     (drbayes
      (lazy-if ((uniform) . < . (const #i499/1000))
               (let ([x  (normal)])
                 (list x (normal x)))
               (let ([x  (cauchy)])
                 (list x (cauchy x)))))))
  (define B (list-rect reals (interval 0.9 1.1 #t #t))))

#;; Test: Bernoulli(p) distribution
;; Preimage should be [0,p)
(begin
  (define p #i2/5)
  (define f-expr (list/arr (random-boolean/arr p)))
  (define B (list-rect 't)))

#;; Test: Geometric(p) distribution
(begin
  (define p #i2/5)
  
  (define/drbayes (geometric-p)
    (lazy-if (boolean (const p)) 0 (+ 1 (geometric-p))))
  
  #;
  (define/drbayes (geometric-p)
    (lazy-if ((uniform) . < . (const p)) 0 (+ 1 (geometric-p))))
  
  (define f-expr
    (program/exp
     (drbayes (geometric-p))))
  
  (define B (interval 1.0 3.0 #t #t))
  
  (let ([xs  (filter (λ: ([x : Flonum]) (<= 1.0 x 3.0)) (sample (geometric-dist p) 100000))])
    (printf "E[x] = ~v~n" (mean xs))
    (printf "sd[x] = ~v~n" (stddev xs))))

#;; Test: Conditioning on Geometric(0.5) distribution defined via recursion
;; Image points should lie on integer x coordinates and be clustered around 3
(begin
  (define p #i499/1000)
  
  (define/drbayes (geometric-p)
    (lazy-cond [((uniform) . >= . (const p))  (+ 1 (geometric-p))]
               [else  0]))
  
  (define f-expr
    (program/exp
     (drbayes
      (let ([x  (geometric-p)])
        (list x (normal x))))))
  
  (define B (list-rect reals (interval 2.9 3.1 #t #t)))
  
  (let ()
    (define xs (sample (geometric-dist p) 10000))
    (define ws (map (λ: ([x : Flonum]) (pdf (normal-dist x) 3.0)) xs))
    (print
     (plot (density (sample (discrete-dist xs ws) (length xs)))
           #:x-label "x" #:y-label "density"))
    (newline)
    (printf "E[x] = ~v~n" (mean xs (ann ws (Sequenceof Real))))
    (printf "sd[x] = ~v~n" (stddev xs (ann ws (Sequenceof Real))))))

;; Test: Normal-Normal model with more observations
;; Density plot, mean, and stddev should be similar to those produced by `normal-normal/lw'
(begin
  (define f-expr
    (program/exp
     (drbayes
      (let ([x  (normal)])
        (list x
              (normal x)
              (normal x)
              (normal x)
              (normal x)
              (normal x)
              (normal x))))))
  (define B (list-rect reals
                       (interval 2.2 2.4 #t #t)
                       (interval 0.9 1.1 #t #t)
                       (interval -0.1 0.1 #t #t)
                       (interval -0.9 -0.7 #t #t)
                       (interval 0.4 0.6 #t #t)
                       (interval 1.3 1.5 #t #t)))
  (normal-normal/lw 0 1 '(2.3 1.0 0.0 -0.8 0.5 1.4) '(1.0 1.0 1.0 1.0 1.0 1.0)))

;; ===================================================================================================

(match-define (meaning idxs f-fwd f-comp) (run-expression f-expr))

(define refine
  (cond [(empty-set? B)  (error 'empty-image)]
        [else  (preimage-refiner f-comp B)]))

(define-values (Ω bs)
  (let-values ([(Ω bs)  (refine (omega-rect) empty-branches)])
    (cond [(empty-set? Ω)  (error 'empty-preimage)]
          [else  (values Ω bs)])))

(printf "idxs = ~v~n" idxs)
(printf "Ω = ~v~n" Ω)
(printf "bs = ~v~n" bs)
(newline)

(struct: domain-sample ([Ω : Omega-Rect]
                        [branches : Branches]
                        [point : Omega]
                        [image-point : (U Void Value)]
                        [measure : Flonum]
                        [prob : Flonum]
                        [weight : Flonum])
  #:transparent)

(: accept-sample? (domain-sample -> Boolean))
(define (accept-sample? s)
  (define x (domain-sample-image-point s))
  (and (not (void? x)) (rect-member? B x)))

(: orig-samples (Listof Omega-Sample))
(define orig-samples
  (time
   ;profile-expr
   (let: loop : (Listof Omega-Sample) ([ss : (Listof Omega-Sample)  empty] [i : Natural  0])
     (when (= 0 (remainder (+ i 1) 100))
       (printf "i = ~v~n" (+ i 1))
       (flush-output))
     (cond [(i . < . n)  (define s (refinement-sample Ω bs idxs refine))
                         (loop (cons s ss) (+ i 1))]
           [else  ss]))))
(newline)

(: all-samples (Listof domain-sample))
(define all-samples
  (for/list: : (Listof domain-sample) ([s  (in-list orig-samples)])
    (match-define (weighted-sample (cons Ω bs) p) s)
    (define ω (omega-rect-sample-point Ω))
    (define x (with-handlers ([if-bad-branch?  (λ (_) (void))])
                (f-fwd ω bs)))
    (define m (omega-rect-measure Ω))
    (domain-sample Ω bs ω x m p (/ m p))))

(define samples (filter accept-sample? all-samples))
(define ws (map domain-sample-weight samples))
(define ms (map domain-sample-measure samples))

(define not-samples (filter (compose not accept-sample?) all-samples))

(define num-all-samples (length all-samples))
(define num-samples (length samples))
(define num-not-samples (length not-samples))

(define accept-prob (fl (/ num-samples num-all-samples)))

(print-sampler-stats)
(newline)

(printf "unique numbers of primitive rvs: ~v~n"
        (sort
         (remove-duplicates
          (map (λ: ([s : domain-sample])
                 (length (hash-keys (omega-rect-hash (domain-sample-Ω s)))))
               all-samples))
         <))
(newline)

(printf "accepted samples: ~v (~v%)~n" (length samples) (* 100.0 accept-prob))
(newline)

(define all-alpha (min 1.0 (/ 250.0 (fl num-all-samples))))
(define alpha (min 1.0 (/ 250.0 (fl num-samples))))

(plot3d (list (rectangles3d (map (compose omega-rect->plot-rect domain-sample-Ω) not-samples)
                            #:alpha all-alpha #:color 1 #:line-color 1)
              (rectangles3d (map (compose omega-rect->plot-rect domain-sample-Ω) samples)
                            #:alpha all-alpha #:color 3 #:line-color 3))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "x1" #:y-label "x2" #:z-label "x3")

(plot3d (list (points3d (map (compose omega->point domain-sample-point) not-samples)
                        #:sym 'dot #:size 12 #:alpha all-alpha #:color 1 #:fill-color 1)
              (points3d (map (compose omega->point domain-sample-point) samples)
                        #:sym 'dot #:size 12 #:alpha all-alpha #:color 3 #:fill-color 3))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "x1" #:y-label "x2" #:z-label "x3")

(plot3d (points3d (sample (discrete-dist (map (compose omega->point domain-sample-point) samples) ws)
                          num-samples)
                  #:sym 'dot #:size 12 #:alpha alpha)
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "x1" #:y-label "x2" #:z-label "x3")

(: xss (Listof (Listof Flonum)))
(define xss
  (map (λ: ([s : domain-sample])
         (define lst (value->listof-flonum (cast (domain-sample-image-point s) Value)))
         (maybe-pad-list lst 3 random))
       samples))

(with-handlers ([exn?  (λ (_) (printf "image points scatter plot failed~n"))])
  (plot3d (points3d xss #:sym 'dot #:size 12 #:alpha alpha)
          #:x-label "x1" #:y-label "x2" #:z-label "x3"))

(with-handlers ([exn?  (λ (_) (printf "resampled image points scatter plot failed~n"))])
  (plot3d (points3d (sample (discrete-dist xss ws) num-samples)
                    #:sym 'dot #:size 12 #:alpha alpha)
          #:x-label "x1" #:y-label "x2" #:z-label "x3"))

(define x0s (map (inst first Flonum Flonum) xss))

(with-handlers ([exn?  (λ (_) (printf "weight density plot failed~n"))])
  (plot (density ws) #:x-label "weight" #:y-label "density"))

(with-handlers ([exn?  (λ (_) (printf "weight/measure scatter plot failed~n"))])
  (plot (points (map (λ: ([w : Flonum] [m : Flonum]) (list w m)) ws ms)
                #:sym 'dot #:size 12 #:alpha alpha)
        #:x-label "weight" #:y-label "measure"))

(printf "Corr(W,M) = ~v~n" (correlation ws ms))

(plot (density (sample (discrete-dist x0s ws) num-samples))
      #:x-label "x0" #:y-label "density")

(printf "E[x0] = ~v~n" (mean x0s (ann ws (Sequenceof Real))))
(printf "sd[x0] = ~v~n" (stddev x0s (ann ws (Sequenceof Real))))