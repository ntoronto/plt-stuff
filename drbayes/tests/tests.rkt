#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         "../main.rkt"
         "test-utils.rkt"
         "profile.rkt"
         "normal-normal.rkt")

(printf "starting...~n~n")

(error-print-width 1024)

(interval-max-splits 5)
(define n 1000)

#;; Test: constants
;; Preimage should be unrestricted
(begin
  (define f-expr (drbayes #f))
  (define B falses))

#;; Test: uniform random
;; Preimage should be [0.25,0.5]
(begin
  (define f-expr random/arr)
  (define B (real-set 0.25 0.5 #t #t)))

#;
(begin
  (interval-max-splits 1)
  (define f-expr (pair/arr random/arr random/arr))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-pair I I)))

#;
(begin
  (interval-max-splits 1)
  (define f-expr (pair/arr random/arr (pair/arr random/arr random/arr)))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-pair I (set-pair I I))))

#;; Test: list/arr (really pair/arr and unit/arr) random/arr
;; Preimage is a rectangle [0.25,0.5] × [0.25,0.5] × [0.25,0.5]
(begin
  (define f-expr (list/arr random/arr random/arr random/arr))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;; Test: same as above, using expressions
(begin
  (define f-expr (drbayes (list (uniform) (uniform) (uniform))))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;
(begin
  (define f-expr (rcompose/arr random/arr sqr/arr))
  (define B (real-set 0.25 0.5 #t #t)))

#;; Test: list/arr random/arr ap/arr sqr/arr
;; Preimage is a rectangle [0.25,0.5] × [0.5,sqrt(1/2)] × [0.5,sqrt(1/2)]
(begin
  (define f-expr
    (list/arr random/arr
              (rcompose/arr random/arr sqr/arr)
              (rcompose/arr random/arr sqr/arr)))
  (define I (real-set 0.25 0.5))
  (define B (set-list I I I)))

#;; Test: same as above, using expressions
(begin
  (interval-max-splits 1)
  (define f-expr (drbayes (list (uniform) (sqr (uniform)) (sqr (uniform)))))
  (define I (real-set 0.25 0.5))
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
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;
(begin
  (define f-expr (drbayes (list (uniform) (/ (uniform)) (/ (uniform)))))
  (define I (real-set 2.0 4.0))
  (define B (set-list (real-set 0.25 0.5) I I)))

#;; Test: list/arr random/arr ap/arr ref/arr +/arr
;; Preimage is a 2D downard diagonal strip × [0,1]
(begin
  (interval-max-splits 3)
  
  (define f-expr
    (rcompose/arr (list/arr random/arr
                            random/arr)
                  (list/arr (ref/arr 0)
                            (ref/arr 1)
                            (rcompose/arr (pair/arr (ref/arr 0) (ref/arr 1)) +/arr))))
  (define B (set-list reals reals (real-set 0.45 0.7))))

#;; Test: same as above, with expressions
(begin
  (interval-max-splits 3)
  
  (define f-expr
    (drbayes
     (let ([x  (uniform)]
           [y  (uniform)])
       (list x y (+ x y)))))
  
  (define B (set-list reals reals (real-set 0.45 0.7))))

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

(begin
  (interval-max-splits 5)
  (define f-expr
    (drbayes (+ (uniform) (uniform))))
  (define B (real-set 0.3 0.7)))

#;; Test: less than
;; Preimage should be:
;;    #t: upper triangle (points above y = x)
;;    #f: lower triangle (points below y = x)
;;  both: [0,1] × [0,1]
(begin
  (interval-max-splits 3)
  (define f-expr (list/arr (rcompose/arr (pair/arr random/arr random/arr) lt/arr)))
  (define B (set-list trues))
  ;(define B (set-list falses))
  ;(define B (set-list booleans))
  )

#;; Test: random boolean
;; Preimage should be [0,0.4)
(begin
  (interval-max-splits 1)
  (define f-expr (drbayes (boolean (const 0.4))))
  (define B trues))

#;; Test: simplest if
;; Preimage should be unrestricted
(begin
  (interval-max-splits 0)
  (define f-expr (drbayes (lazy-if #t #t #f)))
  (define B trues))

#;; Test: simple if
;; Preimage should be [0,0.4]
(begin
  (interval-max-splits 1)
  (define f-expr (drbayes (lazy-if (boolean (const 0.4)) #t #f)))
  (define B trues))

#;
(begin
  (interval-max-splits 0)
  (define f-expr (drbayes (lazy-if ((uniform) . < . 0.3) #t #f)))
  (define B trues))

#;; Test: List length distributed Geometric(0.5)
(begin
  (interval-max-splits 2)
  
  (define/drbayes (S)
    (lazy-if (boolean (const 0.5)) (cons #t (S)) null))
  
  (define f-expr (drbayes (S)))
  (define B universe))

#;; Test: if
;; Preimage should be the union of a large upper triangle and a small lower triangle, and
;; samples should be uniformly distributed
(begin
  (interval-max-splits 4)
  
  (define f-expr
    (drbayes
     (let ([x  (uniform)]
           [y  (uniform)])
       ;(list x y (prim-if (< x y) #t (> x (scale y (const 8)))))
       (list x y (lazy-if (< x y) #t (> x (scale y (const 8)))))
       )))
  (define B (set-list reals reals trues)))

#;; Test: arithmetic
(begin
  (interval-max-splits 4)
  
  (define f-expr
    (drbayes
     (let* ([x  (cauchy)]
            [y  (cauchy)])
       (list x y (/ x y)))))
  
  (define B (set-list reals reals (real-set -0.1 0.2))))

#;; Test: sqr
;; Preimage should look like the graph of the function
(begin
  (interval-max-splits 1)
  
  (define f-expr
    (drbayes
     (let ([x  (uniform -1 1)]
           [y  (uniform 0 1)])
       (list x y (- y (sqr x))))))
  
  (define B (set-list reals reals (real-set -0.1 0.1))))

#;; Test: sine and cosine restricted to [-π,π]
;; Looked at top-down, the plots should look like the graphs of the functions
(begin
  (interval-max-splits 1)
  (define f-expr
    (drbayes
     (let ([x  (uniform (const (- pi)) (const pi))]
           [y  (uniform -1.1 1.1)])
       (list x y (- y
                    ;(partial-cos x)
                    (partial-sin x)
                    )))))
  
  (define B (set-list reals reals (real-set -0.1 0.1))))

#;; Test: asin and acos
;; Looked at top-down, the plots should look like the graphs of the functions
(begin
  (interval-max-splits 1)
  
  (define f-expr
    (drbayes
     (let ([x  (uniform -1 1)]
           [y  ;(uniform 0 (const pi))
               (uniform (const (* -0.5 pi)) (const (* 0.5 pi)))
               ])
       (list x y (- y
                    ;(acos x)
                    (asin x)
                    )))))
  
  (define B (set-list reals reals (real-set -0.1 0.1))))

#;; Test: Normal-Normal model
;; Preimage should be a banana shape
(begin
  (define f-expr
    (rcompose/arr
     (list/arr random/arr random/arr)
     (let* ([X0  (rcompose/arr (ref/arr 0) normal/arr)]
            [X1  (rcompose/arr (pair/arr X0 (rcompose/arr (ref/arr 1) normal/arr)) +/arr)])
       (list/arr X0 X1))))
  (define B (set-list reals (real-set 0.9 1.1)))
  (normal-normal/lw 0 1 '(1.0) '(1.0)))

#;; Test: Normal-Normal model, using expressions
;; Preimage should be as above
(begin
  (interval-max-splits 2)
  ;(interval-min-length (expt 0.5 1.0))
  
  (define f-expr
    (drbayes
     (let* ([x  (normal)]
            [y  (normal x)])
       (list x y))))
  (define B (set-list reals (real-set 0.9 1.1)))
  (normal-normal/lw 0 1 '(1.0) '(1.0)))

#;; Test: thermometer that goes to 100
(begin
  (interval-max-splits 5)
  (interval-min-length 0.0)
  (define f-expr
    (drbayes
     (let* ([x  (normal 90 10)]
            [y  (- x (normal))])
       (list x
             (prim-if (y . > . 100) 100 y)
             ;(lazy-if (y . > . 100) 100 y)
             ))))
  
  (define B (set-list reals (real-set 99.0 100.0))))

#;; Test: Normal-Normal model with circular condition
;; Preimage should look like a football set up for a field goal
(begin
  (interval-max-splits 2)
  
  (define f-expr
    (rcompose/arr
     (list/arr random/arr random/arr)
     (let* ([X0  (rcompose/arr (ref/arr 0) normal/arr)]
            [X1  (rcompose/arr (pair/arr X0 (rcompose/arr (ref/arr 1) normal/arr)) +/arr)])
       (list/arr X0 X1 (rcompose/arr (rcompose/arr (pair/arr (rcompose/arr X0 sqr/arr)
                                                             (rcompose/arr X1 sqr/arr))
                                                   +/arr)
                                     sqrt/arr)))))
  (define B (set-list reals reals (real-set 0.99 1.01 #t #t))))

#;; Test: Normal-Normal model with circular condition, using expressions
;; Preimage should be as above
(begin
  (interval-max-splits 4)
  
  (define/drbayes (hypot x y)
    (sqrt (+ (sqr x) (sqr y))))
  
  (define f-expr
    (drbayes
     (let* ([x0  (normal)]
            [x1  (normal x0)])
       (list x0 x1 (hypot x0 x1)))))
  
  (define B (set-list reals reals (real-set 0.95 1.05))))

#;; Same as above, but with obviously repeated variable in condition
(begin
  (interval-max-splits 2)
  
  (define f-expr
    (drbayes
     (let* ([x0  (uniform)]
            [x1  (uniform)])
       (list x0 x1 (sqrt (+ (sqr x0) (sqr (+ x0 x1))))))))
  
  (define B (set-list reals reals (real-set 0.99 1.01))))

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
  (define B (set-list reals (real-set 0.9 1.1 #t #t) bools)))

#;; Test: Normal-Normal or Cauchy-Cauchy, depending on random variable
(begin
  (interval-max-splits 2)
  (define f-expr
    (drbayes
     (lazy-if ((uniform) . < . (const #i499/1000))
              (let ([x  (normal)])
                (list x (normal x)))
              (let ([x  (cauchy)])
                (list x (cauchy x))))))
  (define B (set-list reals (real-set 0.9 1.1))))

#;; Test: Boolean(p) distribution
;; Preimage should be [0,p); sampler should restart only once
(begin
  (define p #i2/5)
  (define f-expr (lazy-if/arr (boolean/arr p) (c/arr #t) (c/arr #f)))
  (define B trues))

#;; Test: Geometric(p) distribution
(begin
  (interval-max-splits 1)
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
  (define B (real-set B-min B-max #t #t))
  
  (let ([xs  (sample (truncated-dist (geometric-dist p) (- B-min 1.0) B-max) 50000)])
    (printf "E[x] = ~v~n" (mean xs))
    (printf "sd[x] = ~v~n" (stddev xs))))

#;; Test: Conditioning on Geometric(0.5) distribution defined via recursion
;; Image points should lie on integer x coordinates and be clustered around 3
(begin
  (interval-max-splits 1)
  
  (define p #i499/1000)
  
  (define/drbayes (geometric-p)
    ;(lazy-if ((uniform) . < . (const p)) 0 (+ 1 (geometric-p)))
    (let ([x  (lazy-if ((uniform) . < . (const p)) 0 (+ 1 (geometric-p)))])
      (prim-if (negative? x) (fail) x))
    )
  
  (define f-expr
    (drbayes
     (let ([x  (geometric-p)])
       (list x (normal x)))))
  
  (define B (set-list reals (real-set 2.9 3.1 #t #t)))
  
  (let ()
    (define xs (sample (geometric-dist p) 10000))
    (define ws (map (λ: ([x : Flonum]) (pdf (normal-dist x) 3.0)) xs))
    (print
     (plot (density (sample (discrete-dist xs ws) (length xs)))
           #:x-label "x" #:y-label "density"))
    (newline)
    (printf "E[x] = ~v~n" (mean xs (ann ws (Sequenceof Real))))
    (printf "sd[x] = ~v~n" (stddev xs (ann ws (Sequenceof Real))))))

#;; Test: Normal-Normal model with more observations
;; Density plot, mean, and stddev should be similar to those produced by `normal-normal/lw'
(begin
  (interval-max-splits 2)
  ;(interval-min-length (flexpt 0.5 5.0))
  
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
    (set-list reals
              (real-set 3.2 3.4 #t #t)
              (real-set 1.9 2.1 #t #t)
              (real-set 0.9 1.1 #t #t)
              (real-set 0.1 0.3 #t #t)
              (real-set 1.4 1.6 #t #t)
              (real-set 2.3 2.5 #t #t)))
  (normal-normal/lw 0 1 '(3.3 2.0 1.0 0.2 1.5 2.4) '(1.0 1.0 1.0 1.0 1.0 1.0)))

#;
(begin
  (interval-max-splits 2)
  (define f-expr
    (drbayes
     (let ([x  (uniform)]
           [y  (uniform)]
           [z  (uniform)])
       (prim-if (x . < . y) (fail) z))))
  (define B universe))

#;; Test: tagging
;; Preimage should be [0,1]
(begin
  (define t0 (make-set-tag 't0))
  (define f-expr (drbayes (tag (uniform) t0)))
  (define B (set-tag reals t0)))

#;; Test: tagging and untagging
;; Preimage should be [0,1]
(begin
  (define t0 (make-set-tag 't0))
  (define f-expr (drbayes (untag (tag (uniform) t0) t0)))
  (define B reals))

#;; Test: Normal-Normal with circular condition and first variable tagged
(begin
  (define t0 (make-set-tag 't0))
  (define f-expr
    (drbayes
     (let* ([x0  (tag (normal) t0)]
            [x1  (normal (untag x0 t0))])
       (list (untag x0 t0) x1 (sqrt (+ (sqr (untag x0 t0)) (sqr x1)))))))
  (define B (set-list reals reals (real-set 0.95 1.05))))

#;; Test: Normal-Normal with circular condition and first variable tagged with probability 0.5
(begin
  (interval-max-splits 0)
  (define t0 (make-set-tag 't0))
  (define f-expr
    (drbayes
     (let* ([x0  (lazy-if ((uniform) . < . 0.45)
                          (tag (normal 0.0 2.0) t0)
                          (normal))]
            [y0  (lazy-if (tag? x0 t0) (* 0.5 (untag x0 t0)) x0)]
            [x1  (normal y0)])
       (list y0 x1 (sqrt (+ (sqr y0) (sqr x1)))))))
  (define B (set-list reals reals (real-set 0.95 1.05))))

;; ===================================================================================================

(match-define (rand-expression-meaning idxs f-fwd f-comp) (run-rand-expression (->rand f-expr)))
(define f-pre (assert (f-comp (nonempty-domain-set (Omega-Leaf) (Omega-Leaf) nulls))
                      nonempty-rand-preimage?))

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
                        [x : Maybe-Value]
                        [z : Branches]
                        [measure : Flonum]
                        [prob : Flonum]
                        [point-prob : Flonum]
                        [weight : Flonum])
  #:transparent)

(: accept-sample? (domain-sample -> Boolean))
(define (accept-sample? s)
  (define x (domain-sample-x s))
  (and (not (bottom? x))
       (set-member? B x)))

(: orig-samples (Listof omega-rect-sample))
(define orig-samples
  (time
   ;profile-expr
   (refinement-sample* Ω Z idxs refine n)))

(: all-samples (Listof domain-sample))
(define all-samples
  (time
   ;profile-expr
   (let: loop : (Listof domain-sample) ([orig-samples : (Listof omega-rect-sample)  orig-samples])
     (cond
       [(empty? orig-samples)  empty]
       [else
        (define s (first orig-samples))
        (match-define (omega-rect-sample Ω Z m p) s)
        (define pt (refinement-sample-point Ω Z idxs refine))
        ;(define ω (omega-rect-sample-point Ω))
        ;(define z (branches-rect-sample-point Z))
        ;(define pt (omega-sample ω z m))
        (match pt
          [(omega-sample ω z q)
           (define x (f-fwd ω z null))
           (cons (domain-sample Ω Z ω x z m p q (/ q p)) (loop (rest orig-samples)))]
          [_
           (define ω (omega-rect-sample-point Ω))
           (define z (branches-rect-sample-point Z))
           (define x (bottom (delay "refinement-sample-point failed")))
           (cons (domain-sample Ω Z ω x z m p m (/ m p)) (loop (rest orig-samples)))])]))))

(newline)

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

(plot-z-ticks no-ticks)
(plot3d (list (rectangles3d (append*
                             (map (λ: ([s : domain-sample])
                                    (omega-rect->plot-rects (domain-sample-Ω s)))
                                  not-samples))
                            #:alpha all-alpha #:color 1 #:line-color 1)
              (rectangles3d (append*
                             (map (λ: ([s : domain-sample])
                                    (omega-rect->plot-rects (domain-sample-Ω s)))
                                  samples))
                            #:alpha all-alpha #:color 3 #:line-color 3))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "Ω[1/8]" #:y-label "Ω[3/8]" #:z-label ""
        #:angle 0 #:altitude 90)

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

(with-handlers ([exn?  (λ (_) (printf "density plot failed~n"))])
  (plot (density (sample (discrete-dist x0s ws) num-samples) 2)
        #:x-label "x0" #:y-label "density"))

(printf "E[x0] = ~v~n" (mean x0s (ann ws (Sequenceof Real))))
(printf "sd[x0] = ~v~n" (stddev x0s (ann ws (Sequenceof Real))))
