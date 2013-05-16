#lang typed/racket

(struct: Full-Omega () #:transparent)
(struct: Omega-Node ([value : (U Flonum Boolean)] [left : Omega] [right : Omega]) #:transparent)

(define-type Omega (Boxof (U Full-Omega Omega-Node)))

(: make-omega (-> Omega))
(define (make-omega)
  (box (Full-Omega)))

(: omega-uniform-value (Omega -> Flonum))
(define (omega-uniform-value x)
  (define ω (unbox x))
  (cond [(Full-Omega? ω)  (define v (random))
                          (set-box! x (Omega-Node v (make-omega) (make-omega)))
                          v]
        [else
         (define v (Omega-Node-value ω))
         (cond [(flonum? v)  v]
               [else  (raise-argument-error 'omega-uniform-value "Flonum" v)])]))

(: omega-branch-value (Omega -> Boolean))
(define (omega-branch-value x)
  (define ω (unbox x))
  (cond [(Full-Omega? ω)  (define v ((random) . < . 0.5))
                          (set-box! x (Omega-Node v (make-omega) (make-omega)))
                          v]
        [else
         (define v (Omega-Node-value ω))
         (cond [(boolean? v)  v]
               [else  (raise-argument-error 'omega-branch-value "Boolean" v)])]))

(: omega-left (Omega -> Omega))
(define (omega-left x)
  (define ω (unbox x))
  (cond [(Full-Omega? ω)  (define l (make-omega))
                          (set-box! x (Omega-Node (random) l (make-omega)))
                          l]
        [else  (Omega-Node-left ω)]))

(: omega-right (Omega -> Omega))
(define (omega-right x)
  (define ω (unbox x))
  (cond [(Full-Omega? ω)  (define r (make-omega))
                          (set-box! x (Omega-Node (random) (make-omega) r))
                          r]
        [else  (Omega-Node-right ω)]))

;; ===================================================================================================
;; Function arrow

(: id (All (A) (A -> A)))
(define (id x) x)

(: const (All (B) (B -> (Any -> B))))
(define ((const x) _) x)

(: pair (All (A B C) ((A -> B) (A -> C) -> (A -> (Pair B C)))))
(define ((pair f1 f2) x)
  (cons (f1 x) (f2 x)))

(: >>> (All (A B C) ((A -> B) (B -> C) -> (A -> C))))
(define ((>>> f g) x)
  (g (f x)))

(: fst (All (A B) ((Pair A B) -> A)))
(define fst car)

(: snd (All (A B) ((Pair A B) -> B)))
(define snd cdr)

(: add ((Pair Flonum Flonum) -> Flonum))
(define (add x)
  (+ (car x) (cdr x)))

(: omega (All (A) ((A -> (U Flonum Boolean)) (A -> Omega) (A -> Omega) -> (A -> Omega))))
(define ((omega v l r) x)
  (box (Omega-Node (v x) (l x) (r x))))

((pair (>>> ((inst pair (Pair Flonum Flonum) Flonum Flonum) fst snd) add) (const null))
 (cons 5.2 6.0))

((omega omega-uniform-value omega-left omega-right) (make-omega))
((omega omega-branch-value omega-left omega-right) (make-omega))

(: lazy-if (All (A B) ((A -> Boolean) (A -> B) (A -> B) -> (A -> B))))
(define ((lazy-if c t f) x)
  (if (c x) (t x) (f x)))

(: bool= ((Pair Boolean Boolean) -> Boolean))
(define (bool= x)
  (match x
    [(cons #t #t)  #t]
    [(cons #f #f)  #t]
    [_  #f]))

(: assert-bool= ((Pair Boolean Boolean) -> Boolean))
(define assert-bool=
  (lazy-if bool=
           (inst fst Boolean Boolean)
           (λ (bz) (error 'if+ "bad branch: ~e" bz))))

;; ===================================================================================================
;; Randomness arrow, implemented in terms of the function arrow
;; This pairs a random source with only the input to a function

(define-type (Computation+ A B) ((Pair Omega A) -> B))

(: arr+ (All (A B) ((A -> B) -> (Computation+ A B))))
;; Complexity: +2 combinators
(define (arr+ f)
  (>>> (inst snd Omega A) f)
  #;; Without type annotations:
  (>>> snd f))

(: id+ (All (A) (Computation+ A A)))
;; Complexity: +0 combinators
(define (id+ γ) ((inst snd Omega A) γ))

(: const+ (All (B) (B -> (Computation+ Any B))))
;; Complexity: +0 combinators
(define const+ const)

(: >>>+ (All (A B C) ((Computation+ A B) (Computation+ B C) -> (Computation+ A C))))
;; Complexity: +11 combinators
(define (>>>+ f g)
  (>>> (pair (>>> (inst fst Omega A) omega-right)
             (>>> (pair (>>> (inst fst Omega A) omega-left)
                        (inst snd Omega A))
                  f))
       g)
  #;; Without type annotations:
  (>>> (pair (>>> fst omega-right)
             (>>> (pair (>>> fst omega-left) snd) f))
       g))

(: pair+ (All (A B C) ((Computation+ A B) (Computation+ A C) -> (Computation+ A (Pair B C)))))
;; Complexity: +14 combinators
(define (pair+ f1 f2)
  (pair (>>> (pair (>>> (inst fst Omega A) omega-left)  (inst snd Omega A)) f1)
        (>>> (pair (>>> (inst fst Omega A) omega-right) (inst snd Omega A)) f2))
  #;; Without type annotations:
  (pair (>>> (pair (>>> fst omega-left)  snd) f1)
        (>>> (pair (>>> fst omega-right) snd) f2)))

(: uniform+ (Computation+ Any Flonum))
;; Complexity: +2 combinators
(define uniform+
  (>>> (inst fst Omega Any) omega-uniform-value)
  #;; Without type annotations:
  (>>> fst omega-value))

(: if+ (All (A B) ((Computation+ A Boolean) (Computation+ A B) (Computation+ A B)
                                            -> (Computation+ A B))))
(define (if+ c t f)
  (define ω (inst fst Omega A))
  (define x (inst snd Omega A))
  (lazy-if (>>> (pair (>>> (pair (>>> ω omega-left) x) c)
                      (>>> ω omega-branch-value))
                assert-bool=)
           (>>> (pair (>>> (>>> ω omega-right) omega-left) x) t)
           (>>> (pair (>>> (>>> ω omega-right) omega-right) x) f)))

((if+ (const #t) (const 1.0) (const 2.0))
 (cons (box (ann (Omega-Node #t (make-omega) (make-omega)) (U Full-Omega Omega-Node)))  null))

;; ===================================================================================================
;; Another randomness arrow, implemented in terms of the function arrow
;; This pairs a random source with both the input and output of a function

(define-type (Computation* A B) ((Pair Omega A) -> (Pair Omega B)))

(: arr* (All (A B) ((A -> B) -> (Computation* A B))))
;; Complexity: +4 combinators
(define (arr* f)
  (pair (inst fst Omega A) (>>> (inst snd Omega A) f))
  #;; Without type annotations:
  (pair fst (>>> snd f)))

(: id* (All (A) (Computation* A A)))
;; Complexity: +0 combinators
(define (id* γ) γ)

(: const* (All (B) (B -> (Computation* Any B))))
;; Complexity: +2 combinators
(define (const* x)
  ((inst pair (Pair Omega Any) Omega B) fst (const x))
  #;; Without type annotations:
  (pair fst (const x)))

(: >>>* (All (A B C) ((Computation* A B) (Computation* B C) -> (Computation* A C))))
;; Complexity: +32 combinators
(define (>>>* f g)
  (>>> (pair (>>> (inst fst Omega A) omega-right)
             (>>> (pair (>>> (inst fst Omega A) omega-left) (inst snd Omega A)) f))
       (>>> (pair (>>> (pair (inst fst Omega (Pair Omega B))
                             (>>> (inst snd Omega (Pair Omega B))
                                  (inst snd Omega B)))
                       g)
                  (>>> (inst snd Omega (Pair Omega B))
                       (inst fst Omega B)))
            (pair (omega (const +nan.0)
                         (inst snd (Pair Omega C) Omega)
                         (>>> (inst fst (Pair Omega C) Omega)
                              (inst fst Omega C)))
                  (>>> (inst fst (Pair Omega C) Omega)
                       (inst snd Omega C)))))
  #;; Without type annotations:
  (>>> (pair (>>> fst omega-right)
             (>>> (pair (>>> fst omega-left) snd) f))
       (>>> (pair (>>> (pair fst (>>> snd snd)) g)
                  (>>> snd fst))
            (pair (omega (const +nan.0) snd (>>> fst fst))
                  (>>> fst snd)))))

(: pair* (All (A B C) ((Computation* A B) (Computation* A C) -> (Computation* A (Pair B C)))))
;; Complexity: +30 combinators
(define (pair* f1 f2)
  (>>> (pair (>>> (pair (>>> (inst fst Omega A) omega-left)  (inst snd Omega A)) f1)
             (>>> (pair (>>> (inst fst Omega A) omega-right) (inst snd Omega A)) f2))
       (pair (omega (const +nan.0)
                    (>>> (inst fst (Pair Omega B) (Pair Omega C))
                         (inst fst Omega B))
                    (>>> (inst snd (Pair Omega B) (Pair Omega C))
                         (inst fst Omega C)))
             (pair (>>> (inst fst (Pair Omega B) (Pair Omega C))
                        (inst snd Omega B))
                   (>>> (inst snd (Pair Omega B) (Pair Omega C))
                        (inst snd Omega C)))))
  #;; Without type annotations:
  (>>> (pair (>>> (pair (>>> fst omega-left)  snd) f1)
             (>>> (pair (>>> fst omega-right) snd) f2))
       (pair (omega (const +nan.0) (>>> fst fst) (>>> snd fst))
             (pair (>>> fst snd) (>>> snd snd)))))

(: uniform* (Computation* Any Flonum))
;; Complexity: +4 combinators
(define uniform*
  (pair (inst fst Omega Any)
        (>>> (inst fst Omega Any) omega-uniform-value))
  #;; Without type annotations:
  (pair fst (>>> fst omega-value)))

(((inst pair* Null Flonum Flonum) (const* 4.5) (const* 1.2))
 (cons (make-omega) null))

((pair* uniform* uniform*) (cons (make-omega) null))

((>>>* (pair* uniform* uniform*) (arr* add))
 (cons (make-omega) null))
