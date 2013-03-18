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

#;
(begin
  (define f-expr random/arr)
  (define B (interval 0.25 0.5 #t #t)))

#;
(begin
  (define f-expr (pair/arr random/arr random/arr))
  (define I (interval 0.25 0.5 #t #t))
  (define B (set-pair I I)))

#;
(begin
  (define f-expr (pair/arr random/arr (pair/arr random/arr random/arr)))
  (define I (interval 0.25 0.5 #t #t))
  (define B (set-pair I (set-pair I I))))

#;; Test: list/arr (really pair/arr and unit/arr) random/arr
;; Preimage is a rectangle [0.25,0.5] × [0.25,0.5] × [0.25,0.5]
(begin
  (define f-expr (list/arr random/arr random/arr random/arr))
  (define I (interval 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;; Test: same as above, using expressions
(begin
  (define f-expr (drbayes (list (uniform) (uniform) (uniform))))
  (define I (interval 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;
(begin
  (define f-expr (rcompose/arr random/arr sqr/arr))
  (define B (interval 0.25 0.5 #t #t)))

#;; Test: list/arr random/arr ap/arr sqr/arr
;; Preimage is a rectangle [0.25,0.5] × [0.5,sqrt(1/2)] × [0.5,sqrt(1/2)]
(begin
  (define f-expr
    (list/arr random/arr
              (rcompose/arr random/arr sqr/arr)
              (rcompose/arr random/arr sqr/arr)))
  (define I (interval 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;; Test: same as above, using expressions
(begin
  (define f-expr (drbayes (list (uniform) (sqr (uniform)) (sqr (uniform)))))
  (define I (interval 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;; Test: list/arr random/arr ap/arr sqr/arr ref/arr
;; Preimage is the same as just above: [0.25,0.5] × [0.5,sqrt(1/2)] × [0.5,sqrt(1/2)]
(begin
  (define f-expr
    (rcompose/arr (list/arr random/arr
                            (rcompose/arr random/arr sqr/arr)
                            (rcompose/arr random/arr sqr/arr))
                  (list/arr (ref/arr 0)
                            (ref/arr 1)
                            (ref/arr 2))))
  (define I (interval 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;; Test: list/arr random/arr ap/arr ref/arr +/arr
;; Preimage is a 2D downard diagonal strip × [0,1]
(begin
  (define f-expr
    (rcompose/arr (list/arr random/arr
                            random/arr)
                  (list/arr (ref/arr 0)
                            (ref/arr 1)
                            (rcompose/arr (pair/arr (ref/arr 0) (ref/arr 1)) +/arr))))
  (define B (set-list real-interval real-interval (interval 0.45 0.7 #t #t))))

#;; Test: same as above, with expressions
(begin
  (interval-max-splits 2)
  
  (define f-expr
    (drbayes
     (let ([x  (uniform)]
           [y  (uniform)])
       (list x y (+ x y)))))
  
  (define B (set-list real-interval real-interval (interval 0.45 0.7))))

#;; Test: arithmetic
(begin
  (interval-max-splits 4)
  
  (define f-expr
    (drbayes
     (let* ([x  (uniform -1 1)]
            [y  (uniform -1 1)])
       (list x y (* x y)))))
  
  (define B (set-list real-interval real-interval (interval -0.1 0.2))))

#;; Test: boolean #t, #f or both
;; Preimage should be:
;;    #t: [0,0.5)
;;    #f: (0.5,1]
;;  both: [0,1]
(begin
  (define f-expr (list/arr (rcompose/arr (rcompose/arr random/arr normal/arr) negative?/arr)))
  ;(define B (set-list trues))
  (define B (set-list falses))
  ;(define B (set-list booleans))
  )

#;; Test: less than
;; Preimage should be:
;;    #t: upper triangle (points above y = x)
;;    #f: lower triangle (points below y = x)
;;  both: [0,1] × [0,1]
(begin
  (define f-expr (list/arr (rcompose/arr (pair/arr random/arr random/arr) lt/arr)))
  (define B (set-list trues))
  ;(define B (set-list falses))
  ;(define B (set-list booleans))
  )

#;
(begin
  (define f-expr
    (drbayes
     (lazy-if (boolean (const 0.5))
              0.0
              (lazy-if (boolean (const 0.5))
                       1.0
                       (lazy-if (boolean (const 0.5))
                                2.0
                                (fail))))))
  (define B real-interval))

#;; Test: strict-or
;; Preimage should be the union of a large upper triangle and a small lower triangle, and
;; samples should be uniformly distributed
(begin
  (define f-expr
    (drbayes
     (let ([x  (uniform)]
           [y  (uniform)])
       (list x y (prim-if (< x y) #t (> x (scale y (const 8)))))
       ;(list x y (lazy-if (< x y) #t (> x (scale y (const 8)))))
       ;(list x y (strict-if (< x y) #t (> x (scale y (const 8)))))
       )))
  (define B (set-list real-interval real-interval trues)))

#;; Test: Normal-Normal model
;; Preimage should be a banana shape
(begin
  (define f-expr
    (rcompose/arr
     (list/arr random/arr random/arr)
     (let* ([X0  (rcompose/arr (ref/arr 0) normal/arr)]
            [X1  (rcompose/arr (pair/arr X0 (rcompose/arr (ref/arr 1) normal/arr)) +/arr)])
       (list/arr X0 X1))))
  (define B (set-list real-interval (interval 0.9 1.1)))
  (normal-normal/lw 0 1 '(1.0) '(1.0)))

#;; Test: Normal-Normal model, using expressions
;; Preimage should be as above
(begin
  (define f-expr
    (drbayes
     (let* ([x  (normal)]
            [y  (normal x)])
       (list x y))))
  (define B (set-list real-interval (interval 0.9 1.1)))
  (normal-normal/lw 0 1 '(1.0) '(1.0)))

#;; Test: thermometer that goes to 100
(begin
  (interval-max-splits 5)
  (interval-min-length 0.0)
  (define f-expr
    (drbayes
     (let* ([x  (normal 90 10)]
            [y  (- x (normal))])
       (list x (lazy-if (y . > . 100) 100 y)))))
  
  (define B (set-list real-interval (interval 99.0 100.0))))

#;; Test: Normal-Normal model with circular condition
;; Preimage should look like a football set up for a field goal
(begin
  (define f-expr
    (rcompose/arr
     (list/arr random/arr random/arr)
     (let* ([X0  (rcompose/arr (ref/arr 0) normal/arr)]
            [X1  (rcompose/arr (pair/arr X0 (rcompose/arr (ref/arr 1) normal/arr)) +/arr)])
       (list/arr X0 X1 (rcompose/arr (rcompose/arr (pair/arr (rcompose/arr X0 sqr/arr)
                                                             (rcompose/arr X1 sqr/arr))
                                                   +/arr)
                                     sqrt/arr)))))
  (define B (set-list real-interval real-interval (interval 0.95 1.05 #t #t))))

#;; Test: Normal-Normal model with circular condition, using expressions
;; Preimage should be as above
(begin
  (define f-expr
    (drbayes
     (let* ([x0  (normal)]
            [x1  (normal x0)])
       (list x0 x1 (sqrt (+ (sqr x0) (sqr x1)))))))
  (define B (set-list real-interval real-interval (interval 0.95 1.05))))

#;; Test: Normal-Normal or Cauchy-Cauchy, depending on random variable
(begin
  (define f-expr
    (rcompose/arr
     (list/arr random/arr random/arr random/arr)
     (let* ([B  (rcompose/arr (rcompose/arr (pair/arr (ref/arr 0) (c/arr #i499/1000)) -/arr)
                              negative?/arr)]
            [X0  (strict-if/arr
                  B
                  (rcompose/arr (ref/arr 1) normal/arr)
                  (rcompose/arr (ref/arr 1) cauchy/arr))]
            [X1  (strict-if/arr
                  B
                  (rcompose/arr (pair/arr X0 (rcompose/arr (ref/arr 2) normal/arr)) +/arr)
                  (rcompose/arr (pair/arr X0 (rcompose/arr (ref/arr 2) cauchy/arr)) +/arr))])
       (list/arr X0 X1 B))))
  (define B (set-list real-interval (interval 0.9 1.1 #t #t) booleans)))

#;; Test: Normal-Normal or Cauchy-Cauchy, depending on random variable
(begin
  (define f-expr
    (drbayes
     (lazy-if ((uniform) . < . (const #i499/1000))
              (let ([x  (normal)])
                (list x (normal x)))
              (let ([x  (cauchy)])
                (list x (cauchy x))))))
  (define B (set-list real-interval (interval 0.9 1.1))))

#;; Test: Boolean(p) distribution
;; Preimage should be [0,p); sampler should restart only once
(begin
  (define p #i2/5)
  (define f-expr (lazy-if/arr (boolean/arr p) (c/arr #t) (c/arr #f)))
  (define B trues))

#;; Test: Geometric(p) distribution
(begin
  (interval-max-splits 0)
  (define p #i1/16)
  
  (define/drbayes (geometric-p)
    ;(lazy-if (boolean (const p)) 0 (+ 1 (geometric-p)))
    ;; Forces backtracking earlier:
    (let ([x  (lazy-if (boolean (const p)) 0 (+ 1 (geometric-p)))])
      (prim-if (negative? x) (fail) x)))
  
  #;
  (define/drbayes (geometric-p)
    ;(lazy-if ((uniform) . < . (const p)) 0 (+ 1 (geometric-p)))
    ;; Forces backtracking earlier
    (let ([x  (lazy-if ((uniform) . < . (const p)) 0 (+ 1 (geometric-p)))])
      (prim-if (negative? x) (fail) x)))
  
  (define f-expr
    (drbayes (geometric-p)))
  
  (define B-min 1.0)
  (define B-max 3.0)
  (define B (interval B-min B-max #t #t))
  
  (let ([xs  (sample (truncated-dist (geometric-dist p) (- B-min 1.0) B-max) 50000)])
    (printf "E[x] = ~v~n" (mean xs))
    (printf "sd[x] = ~v~n" (stddev xs))))

#;; Test: Conditioning on Geometric(0.5) distribution defined via recursion
;; Image points should lie on integer x coordinates and be clustered around 3
(begin
  (define p #i499/1000)
  
  (define/drbayes (geometric-p)
    (lazy-if ((uniform) . < . (const p)) 0 (+ 1 (geometric-p))))
  
  (define f-expr
    (drbayes
     (let ([x  (geometric-p)])
       (list x (normal x)))))
  
  (define B (set-list real-interval (interval 2.9 3.1 #t #t)))
  
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
  (interval-max-splits 5)
  ;(interval-min-length (flexpt 0.5 14.0))
  
  (define/drbayes (list-append lst1 lst2)
    (lazy-if (null? lst1)
             lst2
             (cons (car lst1) (list-append (cdr lst1) lst2))))
  
  (define f-expr
    (drbayes
     (let ([x  (normal)])
       (list x
             (normal x)
             (normal x)
             (normal x)
             (normal x)
             (normal x)
             (normal x)))))
  (define B
    (set-list real-interval
              (interval 2.2 2.4 #t #t)
              (interval 0.9 1.1 #t #t)
              (interval -0.1 0.1 #t #t)
              (interval -0.9 -0.7 #t #t)
              (interval 0.4 0.6 #t #t)
              (interval 1.3 1.5 #t #t)))
  (normal-normal/lw 0 1 '(2.3 1.0 0.0 -0.8 0.5 1.4) '(1.0 1.0 1.0 1.0 1.0 1.0)))

#;
(begin
  (define/drbayes (S)
    (lazy-if (boolean (const 0.5)) (T) (F)))

  (define/drbayes (T)
    (lazy-cond [(boolean (const 0.4))  (cons #t (T))]
               [(boolean (const 0.5))  (cons #t (F))]
               [else  null]))
  
  (define/drbayes (F)
    (lazy-cond [(boolean (const 0.4))  (cons #f (F))]
               [(boolean (const 0.5))  #;(cons #f (T))
                                       (cons #f (let ([s  (T)])
                                                  s #;(strict-if (list-ref s (const 1))
                                                             (fail)
                                                             s)))]
               [else  null]))
  
  (define f-expr (drbayes (S)))
  
  (define B (set-list* booleans trues falses trues falses trues falses universe)))

#;; Test: tagging
(begin
  (define t0 (make-set-tag 't0))
  (define f-expr (drbayes (tag (uniform) t0)))
  (define B (set-tag real-interval t0)))

#;; Test: tagging and untagging
(begin
  (define t0 (make-set-tag 't0))
  (define f-expr (drbayes (untag (tag (uniform) t0) t0)))
  (define B real-interval))

#;; Test: Normal-Normal with circular condition and first variable tagged
(begin
  (define t0 (make-set-tag 't0))
  (define f-expr
    (drbayes
     (let* ([x0  (tag (normal) t0)]
            [x1  (normal (untag x0 t0))])
       (list (untag x0 t0) x1 (sqrt (+ (sqr (untag x0 t0)) (sqr x1)))))))
  (define B (set-list real-interval real-interval (interval 0.95 1.05))))

#;; Test: Normal-Normal with circular condition and first variable tagged with probability 0.5
(begin
  (define t0 (make-set-tag 't0))
  (define f-expr
    (drbayes
     (let* ([x0  (lazy-if (boolean (const 0.5))
                          (tag (normal 0.0 2.0) t0)
                          (normal))]
            [y0  (prim-if (tag? x0 t0) (* 0.5 (untag x0 t0)) x0)]
            [x1  (normal y0)])
       (list y0
             x1
             (sqrt (+ (sqr y0) (sqr x1)))))))
  (define B (set-list real-interval real-interval (interval 0.95 1.05))))

;; ===================================================================================================

(match-define (expression-meaning idxs f-fwd f-comp) (run-expression f-expr))

(define (empty-set-error)
  (error 'drbayes-sample "cannot sample from the empty set"))

(define refine
  (if (empty-set? B) (empty-set-error) (preimage-refiner f-comp B)))

(define-values (Ω Z)
  (let-values ([(Ω Z)  (refine omega-rect branches-rect)])
    (if (or (empty-set? Ω) (empty-set? Z)) (empty-set-error) (values Ω Z))))

(printf "idxs = ~v~n" idxs)
(printf "Ω = ~v~n" Ω)
(printf "Z = ~v~n" Z)
(newline)

(struct: domain-sample ([Ω : Omega-Rect]
                        [Z : Branches-Rect]
                        [ω : Omega]
                        [x : (U 'if-bad-branch Value)]
                        [z : Branches]
                        [measure : Flonum]
                        [prob : Flonum]
                        [weight : Flonum])
  #:transparent)

(: accept-sample? (domain-sample -> Boolean))
(define (accept-sample? s)
  (define x (domain-sample-x s))
  (and (not (if-bad-branch? x)) (set-member? B x)))

(: orig-samples (Listof omega-sample))
(define orig-samples
  (time
   ;profile-expr
   (refinement-sample* Ω Z idxs refine n)))
(newline)

(: all-samples (Listof domain-sample))
(define all-samples
  (for/list: : (Listof domain-sample) ([s  (in-list orig-samples)])
    (match-define (omega-sample Ω Z m p) s)
    (define ω (omega-rect-sample-point Ω))
    (define z (branches-rect-sample-point Z))
    (define x (with-handlers ([if-bad-branch?  (λ (exn) (assert exn if-bad-branch?))])
                (f-fwd ω z null)))
    (domain-sample Ω Z ω x z m p (/ m p))))

(define samples (filter accept-sample? all-samples))
(define ws (map domain-sample-weight samples))
(define ps (map domain-sample-prob samples))
(define ms (map domain-sample-measure samples))

(define not-samples (filter (compose not accept-sample?) all-samples))

(define num-all-samples (length all-samples))
(define num-samples (length samples))
(define num-not-samples (length not-samples))

(define accept-prob (fl (/ num-samples num-all-samples)))

(printf "search stats:~n")
(get-search-stats)
(newline)

(printf "cache stats:~n")
(get-cache-stats)
(newline)

(printf "unique numbers of primitive rvs: ~v~n"
        (sort
         (remove-duplicates
          (map (λ: ([s : domain-sample])
                 (length (omega-rect-map (domain-sample-Ω s) (λ (_) #t))))
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

(plot3d (list (points3d (map (compose omega->point domain-sample-ω) not-samples)
                        #:sym 'dot #:size 12 #:alpha all-alpha #:color 1 #:fill-color 1)
              (points3d (map (compose omega->point domain-sample-ω) samples)
                        #:sym 'dot #:size 12 #:alpha all-alpha #:color 3 #:fill-color 3))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "x1" #:y-label "x2" #:z-label "x3")

(plot3d (points3d (sample (discrete-dist (map (compose omega->point domain-sample-ω) samples) ws)
                          num-samples)
                  #:sym 'dot #:size 12 #:alpha alpha)
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "x1" #:y-label "x2" #:z-label "x3")

(: xss (Listof (Listof Flonum)))
(define xss
  (map (λ: ([s : domain-sample])
         (define lst (value->listof-flonum (cast (domain-sample-x s) Value)))
         (maybe-pad-list lst 3 random))
       samples))

#|
(define all-Ωs (map domain-sample-Ω all-samples))
(define all-Zs (map domain-sample-Z all-samples))

(define resamples
  (sample (discrete-dist (map (λ: ([s : (weighted-sample (Pair Omega-Rect Branches-Rect))])
                                (weighted-sample-value s))
                              orig-samples)
                         (map domain-sample-weight all-samples))
          n))

(: resampled-xss (Listof (Listof Flonum)))
(define resampled-xss
  (let loop ([ss  resamples])
    (cond [(empty? ss)  empty]
          [else
           (match-define (cons Ω Z) (first ss))
           (define ω (omega-rect-sample-point Ω))
           (define x (with-handlers ([if-bad-branch?  (λ (_) (void))])
                       (f-fwd ω null Z)))
           (cond [(and (not (void? x)) (rect-member? B x))
                  (define lst (value->listof-flonum x))
                  (cons (maybe-pad-list lst 3 random) (loop (rest ss)))]
                 [else
                  (loop (rest ss))])])))
|#

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
