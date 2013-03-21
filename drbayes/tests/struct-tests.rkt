#lang typed/racket

(require plot/typed
         "../main.rkt")

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
(define/drbayes (sphere-surface)
  (vec-norm (list (normal) (normal) (normal))))

(define/drbayes (sphere-surface)
  (let ([θ  (uniform (const (- pi)) (const pi))]
        [φ  (* 2 (asin (sqrt (uniform))))])
    (list (* (partial-sin φ) (partial-cos θ))
          (* (partial-sin φ) (partial-sin θ))
          (partial-cos φ))))

(define/drbayes (sphere-half-surface n)
  (let ([v  (sphere-surface)])
    (lazy-if ((vec-dot n v) . > . 0) v (vec-neg v))))

(struct/drbayes collision (time point data))

(define/drbayes (closer-collision c1 c2)
  (prim-if (collision? c1)
           (prim-if (collision? c2)
                    (prim-if ((collision-time c2) . < . (collision-time c1)) c2 c1)
                    c1)
           c2))

(define/drbayes (ray-reflect d n)
  (vec- d (vec-scale n (* 2.0 (vec-dot d n)))))

(define/drbayes (ray-sphere-intersect p0 d pc r)
  (let* ([dp  (vec- pc p0)]
         [-b/2  (vec-dot dp d)]
         [disc  (+ (sqr -b/2) (- (sqr r) (vec-mag^2 dp)))])
    (lazy-if (disc . > . 0.01)
               (let ([t  (- -b/2 (sqrt disc))])
                 (lazy-if (t . > . 0.01)
                            (let* ([dt  (vec-scale d t)]
                                   [p1  (vec+ p0 dt)]
                                   [n   (vec-scale (vec- p1 pc) (/ r))])
                              (collision t p1 (sphere-half-surface n)))
                            #f))
               #f)))

(define/drbayes (ray-plane-intersect p0 v n d prob)
  (let ([denom  (vec-dot v n)])
    (lazy-if (denom . < . -0.01)
               (let ([t  (- (/ (+ d (vec-dot p0 n)) denom))])
                 (lazy-if (t . > . 0.01)
                            (let ([p1  (vec+ p0 (vec-scale v t))])
                              (collision t p1 null)
                              #;
                              (prim-if ((uniform) . < . prob)
                                       (collision t p1 (sphere-half-surface n))
                                       (collision t p1 null)))
                            #f))
               #f)))

(define plane1-n (list 0.0 1.0 0.0))
(define plane1-d 0.0)
(define plane2-n (list 0.0 1.0 0.0))
(define plane2-d 0.5)

(define sphere0-pc (list 0.4 0.6 0.4))
(define sphere0-r 0.25)


#;
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (ray-sphere-intersect p0 d (const sphere0-pc) (const sphere0-r))])
    (lazy-if (collision? c)
               (cons (collision-point c) ps)
               ps)))


(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (ray-sphere-intersect p0 d (const sphere0-pc) (const sphere0-r))])
    (lazy-if (collision? c)
             (let* ([p0  (collision-point c)]
                    [d  (collision-data c)]
                    [ps  (cons p0 ps)]
                    [c   (ray-plane-intersect p0 d (const plane1-n) (const plane1-d) 0.0)])
               (lazy-if (collision? c)
                        (cons (collision-point c) ps)
                        ps))
             ps)))

#;
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (closer-collision
               (ray-sphere-intersect p0 d (const sphere0-pc) (const sphere0-r))
               (ray-plane-intersect p0 d (const plane1-n) (const plane1-d) 0.0))])
    (strict-if (collision? c)
               (let ([d  (collision-data c)])
                 (lazy-if (null? d)
                          (cons (collision-point c) ps)
                          (trace-light (cons (collision-point c) ps) d)))
               ps)))

(define p0 (list 0.9 0.25 0.9))


(interval-max-splits 8)

(define-values (traces ws)
  (let ()
    (define pws
      (time
       (let ()
         (define-values (ps ws)
           (drbayes-sample (drbayes (trace-light (list (const p0)) (sphere-surface)))
                           4000
                           (set-list* (set-list (interval 0.48 0.52)
                                                (interval -0.001 0.001)
                                                (interval 0.48 0.52))
                                      universe
                                      universe
                                      universe)))
         (map (inst cons Value Flonum) ps ws))))
    (values (cast (map (inst car Value Flonum) pws)
                  (Listof (Listof (List Flonum Flonum Flonum))))
            (map (inst cdr Value Flonum) pws))))

(get-cache-stats)
(get-search-stats)

(plot-background '(0.1 0.1 0.1))
(plot-foreground 'white)

(plot3d (ann (map (λ: ([ps : (Listof (Listof Flonum))])
                    (lines3d ps))
                  traces)
             (Listof renderer3d))
        #:x-min 0.0 #:x-max 1.0
        #:y-min 0.0 #:y-max 1.0
        #:z-min 0.0 #:z-max 1.0)

(plot
 (points (map (λ: ([ps : (Listof (Listof Flonum))])
                (define p0 (second ps))
                (define p1 (first ps))
                (define d (vec- p1 p0))
                (define c (ray-plane-intersect p1 d plane2-n plane2-d 0.0))
                (define p (cast (collision-point c) (Listof Flonum)))
                (list (- 1.0 (first p)) (- 1.0 (third p))))
              traces)
         #:sym 'dot #:size 12 #:alpha 1.0 #:color 'white)
 ;#:x-min 0.4 #:x-max 0.6
 ;#:y-min 0.4 #:y-max 0.6
 )


#|
(begin
  (interval-max-splits 5)
  
  (define f-expr
    (drbayes (trace-light (list (const p0)) (sphere-surface))))
  
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
            [p1  (vec+ (vec-scale (sphere-surface) 0.25) p0)]
            [p2  (vec+ (vec-scale (sphere-surface) 0.25) p1)])
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
