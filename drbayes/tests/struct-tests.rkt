#lang typed/racket

(require plot/typed
         images/flomap
         racket/flonum
         "../main.rkt"
         "test-utils.rkt")

(printf "starting...~n")

(define/drbayes (vec+ lst1 lst2)
  (list (+ (list-ref lst1 (const 0)) (list-ref lst2 (const 0)))
        (+ (list-ref lst1 (const 1)) (list-ref lst2 (const 1)))
        (+ (list-ref lst1 (const 2)) (list-ref lst2 (const 2)))))

(define/drbayes (vec- lst1 lst2)
  (list (- (list-ref lst1 (const 0)) (list-ref lst2 (const 0)))
        (- (list-ref lst1 (const 1)) (list-ref lst2 (const 1)))
        (- (list-ref lst1 (const 2)) (list-ref lst2 (const 2)))))

(define/drbayes (vec-neg lst)
  (list (- (list-ref lst (const 0)))
        (- (list-ref lst (const 1)))
        (- (list-ref lst (const 2)))))

(define/drbayes (vec-mag^2 lst)
  (+ (+ (sqr (list-ref lst (const 0)))
        (sqr (list-ref lst (const 1))))
     (sqr (list-ref lst (const 2)))))

(define/drbayes (vec-dot lst1 lst2)
  (+ (+ (* (list-ref lst1 (const 0)) (list-ref lst2 (const 0)))
        (* (list-ref lst1 (const 1)) (list-ref lst2 (const 1))))
     (* (list-ref lst1 (const 2)) (list-ref lst2 (const 2)))))

(define/drbayes (vec-norm lst)
  (let ([z  (sqrt (vec-mag^2 lst))])
    (list (/ (list-ref lst (const 0)) z)
          (/ (list-ref lst (const 1)) z)
          (/ (list-ref lst (const 2)) z))))

(define/drbayes (vec-scale lst s)
  (list (* (list-ref lst (const 0)) s)
        (* (list-ref lst (const 1)) s)
        (* (list-ref lst (const 2)) s)))

#;; This implementation results in awful image approximations when the denominator in the
;; normalization may be zero
(define/drbayes (uniform-vec)
  (vec-norm (list (normal) (normal) (normal))))

(define/drbayes (kinda-normal)
  (let ([x  (normal)])
    (prim-if (and (x . > . -3) (x . < . 3)) x (fail))))

(define/drbayes (uniform-vec)
  (list (kinda-normal) (kinda-normal) (kinda-normal))
  #;
  (list (normal) (normal) (normal)))

#;; Polar coordinate sampling uses fewer random variables than above, and doesn't do division
(define/drbayes (uniform-vec)
  (let ([θ  (uniform (const (- pi)) (const pi))]
        [φ  (* 2 (asin (sqrt (uniform))))])
    (let ([sin-φ  (partial-sin φ)])
      (list (* sin-φ (partial-cos θ))
            (* sin-φ (partial-sin θ))
            (partial-cos φ)))))

(define/drbayes (uniform-vec/dir n)
  (let ([v  (uniform-vec)])
    (lazy-if ((vec-dot n v) . > . 0) v (vec-neg v))))

(struct/drbayes collision (time point normal))

(define/drbayes (closer-collision c1 c2)
  (lazy-if (and (collision? c1) (collision? c2))
           (lazy-if ((collision-time c2) . < . (collision-time c1)) c2 c1)
           (lazy-if (collision? c1) c1 c2)))

(define/drbayes (ray-reflect d n)
  (vec- d (vec-scale n (* 2.0 (vec-dot d n)))))

(define/drbayes (ray-sphere-intersect p v c r)
  (let* ([dp  (vec- c p)]
         [-b/2  (vec-dot dp v)]
         [disc  (- (sqr -b/2) (- (vec-mag^2 dp) (sqr r)))])
    (lazy-if (positive? disc)
             (let ([t  (- -b/2 (sqrt disc))])
               (lazy-if (positive? t)
                        (let* ([p1  (vec+ p (vec-scale v t))]
                               [n   (vec-scale (vec- p1 c) (/ r))])
                          (collision t p1 n))
                        #f))
             #f)))

(define/drbayes (ray-plane-intersect p0 v n d)
  (let ([denom  (- (vec-dot v n))])
    (lazy-if (positive? denom)
             (let ([t  (/ (+ d (vec-dot p0 n)) denom)])
               (lazy-if (positive? t)
                        (collision t (vec+ p0 (vec-scale v t)) n)
                        #f))
             #f)))

(define plane1-n (list 0.0 1.0 0.0))
(define plane1-d 0.0)
(define plane2-n (list 0.0 -1.0 0.0))
(define plane2-d 1.0)
(define plane3-n (list 1.0 0.0 0.0))
(define plane3-d 0.0)
(define plane4-n (list -1.0 0.0 0.0))
(define plane4-d 1.0)
(define plane5-n (list 0.0 0.0 1.0))
(define plane5-d 0.0)
(define plane6-n (list 0.0 0.0 -1.0))
(define plane6-d 1.0)

(define sphere0-pc (list 0.4 0.6 0.4))
(define sphere0-r 0.25)

#;; Cast one unit in direction d
(define/drbayes (trace-light ps d)
  (cons (vec+ d (car ps)) ps))

#;; Intersection test against plane
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))])
    (lazy-if (collision? c)
             (cons (collision-point c) ps)
             ps)))

#;; Intersection test against sphere
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (ray-sphere-intersect p0 d (const sphere0-pc) (const sphere0-r))])
    (lazy-if (collision? c)
             (cons (collision-point c) ps)
             ps)))


(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (closer-collision
               (closer-collision 
                (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))
                (ray-plane-intersect p0 d (const plane2-n) (const plane2-d)))
               (closer-collision
                (closer-collision
                 (ray-plane-intersect p0 d (const plane3-n) (const plane3-d))
                 (ray-plane-intersect p0 d (const plane4-n) (const plane4-d)))
                (closer-collision
                 (ray-plane-intersect p0 d (const plane5-n) (const plane5-d))
                 (ray-plane-intersect p0 d (const plane6-n) (const plane6-d)))))])
    (lazy-if (collision? c)
             #;(cons (collision-point c) ps)
             
             (let* ([p0  (collision-point c)]
                    [n  (collision-normal c)]
                    [d  (uniform-vec/dir n)]
                    [ps  (cons p0 ps)]
                    [c   (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))])
               (lazy-if (collision? c)
                        (cons (collision-point c) ps)
                        ps))
             ps)))

#;
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (closer-collision
               (ray-sphere-intersect p0 d (const sphere0-pc) (const sphere0-r))
               (ray-plane-intersect p0 d (const plane1-n) (const plane1-d)))])
    (strict-if (collision? c)
               (let ([d  (collision-data c)])
                 (lazy-if (null? d)
                          (cons (collision-point c) ps)
                          (trace-light (cons (collision-point c) ps) d)))
               ps)))

(define p0 (list 0.9 0.25 0.9))

(define/drbayes (start-p)
  (const p0))

(interval-max-splits 0)
;(interval-min-length (expt 0.5 5.0))

(define n 20000)

(define f-expr
  (drbayes (trace-light (list (start-p)) (uniform-vec))))

(define H
  #;
  (set-list reals reals reals)
  
  (set-list (real-set 0.49 0.51)
            (real-set -0.001 0.001)
            (real-set 0.49 0.51)))

(define K
  ;universe
  ;(set-list* H universe universe)
  
  (set-list* H
             universe
             universe
             universe))

(match-define (rand-expression-meaning idxs f-fwd f-comp) (run-rand-expression (->rand f-expr)))

(define refine
  (cond [(empty-set? K)  (error 'refine "K is empty-set")]
        [else  (preimage-refiner f-comp K)]))

(define-values (traces ws)
  (let ()
    (define pws
      (time
       ;profile-expr
       (let ()
         (define-values (ps ws) (drbayes-sample f-expr n K))
         (map (inst cons Value Flonum) ps ws))))
    (values (cast (map (inst car Value Flonum) pws)
                  (Listof (Listof (List Flonum Flonum Flonum))))
            (map (inst cdr Value Flonum) pws))))

(get-cache-stats)
(get-search-stats)

(printf "(length traces) = ~v~n" (length traces))

;(plot-background '(0.1 0.1 0.1))
;(plot-foreground 'white)

(plot3d (ann (map (λ: ([ps : (Listof (Listof Flonum))])
                    (lines3d ps))
                  (if ((length traces) . > . 2000) (take traces 2000) traces))
             (Listof renderer3d))
        #:x-min -0.1 #:x-max 1.1
        #:y-min -0.1 #:y-max 1.1
        #:z-min -0.1 #:z-max 1.1)

(define plane1-ccd-d 0.5)

(define xys
  (map (λ: ([ps : (Listof (Listof Flonum))])
         (define p0 (second ps))
         (define p1 (first ps))
         (define d (vec- p1 p0))
         (define c (ray-plane-intersect p1 d plane1-n plane1-ccd-d))
         (define p (cast (collision-point c) (Listof Flonum)))
         (list (- 1.0 (first p)) (- 1.0 (third p))))
       traces))

(define width 256)
(define height 256)
(define fm (make-flomap 1 width height))
(define vs (flomap-values fm))
(for: ([xy  (in-list xys)]
       [w  (in-list ws)])
  (match-define (list orig-x orig-y) xy)
  (define x (exact-round (* width orig-x)))
  (define y (exact-round (* height (- 1.0 orig-y))))
  (when (and (<= 0 x) (< x width)
             (<= 0 y) (< y height))
    (define i (coords->index 1 width 0 x y))
    (flvector-set! vs i (+ w (flvector-ref vs i)))))

(flomap->bitmap (flomap-normalize (flomap-blur fm 1.0)))

#|
(begin
  (interval-max-splits 5)
  
  (define f-expr
    (drbayes (trace-light (list (const p0)) (uniform-vec))))
  
  (define B
    (set-list* #;(set-list (interval 0.0 1.0)
                        (interval 0.0 0.55)
                        (interval 0.0 1.0))
               
               (set-list (interval 0.4 0.6)
                         (interval -0.001 0.001)
                         (interval 0.4 0.6))
               universe
               universe
               universe)))
#;
(begin
  (interval-max-splits 2)
  
  (define f-expr
    (drbayes
     (let* ([p0  (const sphere0-pc)]
            [p1  (vec+ (vec-scale (uniform-vec) 0.25) p0)]
            [p2  (vec+ (vec-scale (uniform-vec) 0.25) p1)])
       (list p2 p1 p0))))
  
  (define B
    (set-list (set-list (interval 0.0 1.0)
                        (interval 0.0 0.2)
                        (interval 0.0 1.0))
              universe
              universe)))

(match-define (expression-meaning idxs f-fwd f-comp) (run-expression f-expr))

(define (empty-set-error)
  (error 'drbayes-sample "cannot sample from the empty set"))

(define refine
  (if (empty-set? B) (empty-set-error) (preimage-refiner f-comp B)))

(define-values (Ω Z)
  (let-values ([(Ω Z)  (refine omega-rect branches-rect)])
    (if (or (empty-set? Ω) (empty-set? Z)) (empty-set-error) (values Ω Z))))

(match-define (list (omega-sample Ω1 Z1 m1 p1))
  (refinement-sample* Ω Z idxs refine 1))

(define traces
  (filter
   (make-predicate (Listof (List Flonum Flonum Flonum)))
   (build-list
    100
    (λ (_)
      (define ω (omega-rect-sample-point Ω1))
      (define z (branches-rect-sample-point Z1))
      (with-handlers ([forward-fail?  (λ: ([e : forward-fail]) e)])
        (f-fwd ω z null))))))

(plot3d (ann (map (λ: ([ps : (Listof (Listof Flonum))])
                    (lines3d ps))
                  traces)
             (Listof renderer3d))
        #:x-min 0.0 #:x-max 1.0
        #:y-min 0.0 #:y-max 1.0
        #:z-min 0.0 #:z-max 1.0)
|#
