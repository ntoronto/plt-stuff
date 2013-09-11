#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "../untyped-utils.rkt"
         "indexes.rkt"
         "preimage-mapping.rkt"
         "pure-arrows.rkt"
         "pure-lifts.rkt")

(provide (all-defined-out))

(struct: bot-wrapper ([arrow : Bot-Arrow]) #:transparent)
(struct: pre-wrapper ([arrow : Pre-Arrow]) #:transparent)

(define-type Bot*-Arrow (U bot-wrapper (Tree-Index -> Bot-Arrow)))
(define-type Pre*-Arrow (U pre-wrapper (Tree-Index -> Pre-Arrow)))
(define-type Idx-Arrow (Tree-Index -> Indexes))

(: run/bot* (Bot*-Arrow Tree-Index -> Bot-Arrow))
(define (run/bot* k j)
  (cond [(bot-wrapper? k)  ((ref/bot 'snd) . >>>/bot . (bot-wrapper-arrow k))]
        [else  (k j)]))

(: run/pre* (Pre*-Arrow Tree-Index -> Pre-Arrow))
(define (run/pre* k j)
  (cond [(pre-wrapper? k)  ((ref/pre 'snd) . >>>/pre . (pre-wrapper-arrow k))]
        [else  (k j)]))
                             
(: η/bot* (Bot-Arrow -> Bot*-Arrow))
(define η/bot* bot-wrapper)

(: η/pre* (Pre-Arrow -> Pre*-Arrow))
(define η/pre* pre-wrapper)

(: any/idx Idx-Arrow)
(define (any/idx j) '())

;; ===================================================================================================
;; Basic computable lifts

(define fail/bot* (η/bot* fail/bot))
(define fail/pre* (η/pre* fail/pre))
(define fail/idx any/idx)

(define id/bot* (η/bot* id/bot))
(define id/pre* (η/pre* id/pre))
(define id/idx any/idx)

(define restrict/bot* (λ: ([X : Nonempty-Set]) (η/bot* (restrict/bot X))))
(define restrict/pre* (λ: ([X : Nonempty-Set]) (η/pre* (restrict/pre X))))
(define restrict/idx (λ: ([X : Nonempty-Set]) any/idx))

(define const/bot* (λ: ([b : Value]) (η/bot* (const/bot b))))
(define const/pre* (λ: ([b : Value]) (η/pre* (const/pre b))))
(define const/idx (λ: ([b : Value]) any/idx))

(define ref/bot* (λ: ([j : Pair-Index]) (η/bot* (ref/bot j))))
(define ref/pre* (λ: ([j : Pair-Index]) (η/pre* (ref/pre j))))
(define ref/idx (λ: ([j : Pair-Index]) any/idx))

;; ===================================================================================================
;; Arrow combinators

;; ---------------------------------------------------------------------------------------------------
;; Composition

(: >>>/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (>>>/bot* k1 k2)
  (λ: ([j : Tree-Index])
    (((ref/bot 'fst) . &&&/bot . (run/bot* k1 (left j))) . >>>/bot . (run/bot* k2 (right j)))))

(: >>>/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (>>>/pre* k1 k2)
  (λ: ([j : Tree-Index])
    (((ref/pre 'fst) . &&&/pre . (run/pre* k1 (left j))) . >>>/pre . (run/pre* k2 (right j)))))

(: >>>/idx (Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((>>>/idx k1 k2) j)
  (append (k1 (left j)) (k2 (right j))))

;; ---------------------------------------------------------------------------------------------------
;; Pairing

(: &&&/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (&&&/bot* k1 k2)
  (λ: ([j : Tree-Index])
    ((run/bot* k1 (left j)) . &&&/bot . (run/bot* k2 (right j)))))

(: &&&/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (&&&/pre* k1 k2)
  (λ: ([j : Tree-Index])
    ((run/pre* k1 (left j)) . &&&/pre . (run/pre* k2 (right j)))))

(: &&&/idx (Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((&&&/idx k1 k2) j)
  (append (k1 (left j)) (k2 (right j))))

;; ---------------------------------------------------------------------------------------------------
;; Partial if-then-else

(: ifte/bot* (Bot*-Arrow Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (ifte/bot* k1 k2 k3)
  (λ: ([j : Tree-Index])
    (ifte/bot (run/bot* k1 (left j))
              (run/bot* k2 (left (right j)))
              (run/bot* k3 (right (right j))))))

(: ifte/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (ifte/pre* k1 k2 k3)
  (λ: ([j : Tree-Index])
    (ifte/pre (run/pre* k1 (left j))
              (run/pre* k2 (left (right j)))
              (run/pre* k3 (right (right j))))))

(: ifte/idx (Idx-Arrow Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((ifte/idx k1 k2 k3) j)
  (append (k1 (left j))
          (k2 (left (right j)))
          (k3 (right (right j)))))

;; ---------------------------------------------------------------------------------------------------
;; Laziness

(: lazy/bot* ((-> Bot*-Arrow) -> Bot*-Arrow))
(define (lazy/bot* k)
  (λ: ([j : Tree-Index])
    (lazy/bot (λ () (run/bot* (k) j)))))

(: lazy/pre* ((-> Pre*-Arrow) -> Pre*-Arrow))
(define (lazy/pre* k)
  (λ: ([j : Tree-Index])
    (lazy/pre (λ () (run/pre* (k) j)))))

(: lazy/idx ((-> Idx-Arrow) -> Idx-Arrow))
(define ((lazy/idx k) j)
  ((k) j))

;; ===================================================================================================
;; Random source and branch trace projections

(: proj-domain-fail (Symbol Value -> Bottom))
(define (proj-domain-fail name a)
  (bottom
   (delay
     (format
      "~a: expected value in (set-pair (set-pair omegas traces) universe); given ~e" name a))))

;; ---------------------------------------------------------------------------------------------------
;; Branch trace projections

(: branch/bot* Bot*-Arrow)
(define branch/bot*
  (λ: ([j : Tree-Index])
    (let ([j  (reverse j)])
      (λ: ([a : Value])
        (match a
          [(cons (cons (? omega?) (? trace? t)) _)  (trace-ref t j)]
          [_  (proj-domain-fail 'branch a)])))))

(: branch/pre (Tree-Index -> Pre-Arrow))
(define (branch/pre j)
  (let ([j  (reverse j)])
    (λ (A)
      (define T (set-take-traces A))
      (cond [(empty-set? T)  empty-pre-mapping]
            [else  (define B (trace-set-proj T j))
                   (cond [(empty-bool-set? B)  empty-pre-mapping]
                         [else  (nonempty-pre-mapping
                                 B (λ (B) (let ([T  (trace-set-unproj T j (set-take-bools B))])
                                            (cond [(empty-trace-set? T)  empty-set]
                                                  [else  T]))))])]))))

(: branch/pre* Pre*-Arrow)
(define branch/pre*
  (λ: ([j : Tree-Index])
    ((ref/pre 'fst) . >>>/pre . ((ref/pre 'snd) . >>>/pre . (branch/pre j)))))

;; ---------------------------------------------------------------------------------------------------
;; Random source projections

(: random/bot* Bot*-Arrow)
(define random/bot*
  (λ: ([j : Tree-Index])
    (let ([j  (reverse j)])
      (λ: ([a : Value])
        (match a
          [(cons (cons (? omega? r) (? trace?)) _)  (omega-ref r j)]
          [_  (proj-domain-fail 'random a)])))))

(: random/pre (Tree-Index -> Pre-Arrow))
(define (random/pre j)
  (let ([j  (reverse j)])
    (λ (A)
      (define R (set-take-omegas A))
      (cond [(empty-set? R)  empty-pre-mapping]
            [else  (define B (omega-set-proj R j))
                   (cond [(empty-real-set? B)  empty-pre-mapping]
                         [else  (nonempty-pre-mapping
                                 B (λ (B) (let ([R  (omega-set-unproj R j (set-take-reals B))])
                                            (cond [(empty-omega-set? R)  empty-set]
                                                  [else  R]))))])]))))

(: random/pre* Pre*-Arrow)
(define random/pre*
  (λ: ([j : Tree-Index])
    ((ref/pre 'fst) . >>>/pre . ((ref/pre 'fst) . >>>/pre . (random/pre j)))))

(: random/idx Idx-Arrow)
(define (random/idx j)
  (list (random-index (reverse j) #f)))

;; ---------------------------------------------------------------------------------------------------
;; Random source boolean projections

(: boolean/bot* (Flonum -> Bot*-Arrow))
(define (boolean/bot* p)
  (λ: ([j : Tree-Index])
    (define random (run/bot* random/bot* j))
    (λ: ([a : Value])
      (let ([b  (random a)])
        (and (flonum? b) (b . < . p))))))

(: boolean-preimage (Flonum -> (Values Nonextremal-Interval Nonextremal-Interval)))
;; Assumes p > 0.0 and p < 1.0
(define (boolean-preimage p)
  (values (Nonextremal-Interval 0.0 p #t #f)
          (Nonextremal-Interval p 1.0 #t #t)))

(: boolean/pre (Flonum Tree-Index -> Pre-Arrow))
(define (boolean/pre p j)
  (cond [(and (p . > . 0.0) (p . < . 1.0))
         (define-values (It If) (boolean-preimage p))
         (let ([j  (reverse j)])
           (λ (A)
             (define R (set-take-omegas A))
             (define Rj (omega-set-proj R j))
             (let ([It  (real-set-intersect It Rj)]
                   [If  (real-set-intersect If Rj)])
               (define-values (B I)
                 (cond [(and (empty-real-set? It) (empty-real-set? If))
                        (values empty-set empty-real-set)]
                       [(empty-real-set? If)  (values trues It)]
                       [(empty-real-set? It)  (values falses It)]
                       [else  (values bools (real-set-union It If))]))
               (pre-mapping B (λ (B)
                                (let ([R  (cond [(eq? B trues)   (omega-set-unproj R j It)]
                                                [(eq? B falses)  (omega-set-unproj R j If)]
                                                [else  (omega-set-unproj R j I)])])
                                  (if (empty-omega-set? R) empty-set R)))))))]
        [else
         (const/pre (p . >= . 1.0))]))

(: boolean/pre* (Flonum -> Pre*-Arrow))
(define (boolean/pre* p)
  (λ: ([j : Tree-Index])
    ((ref/pre 'fst) . >>>/pre . ((ref/pre 'fst) . >>>/pre . (boolean/pre p j)))))

(: boolean/idx (Flonum -> Idx-Arrow))
(define (boolean/idx p)
  (cond [(and (p . > . 0.0) (p . < . 1.0))
         (define-values (It If) (boolean-preimage p))
         (define split (make-constant-splitter (list It If)))
         (λ (j) (list (random-index (reverse j) split)))]
        [else  any/idx]))

;; ===================================================================================================
;; Total if-then-else

(: ifte*/bot* (Bot*-Arrow Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (ifte*/bot* k1 k2 k3)
  (λ: ([j : Tree-Index])
    (define branch (run/bot* branch/bot* j))
    (define f1 (run/bot* k1 (left j)))
    (define f2 (run/bot* k2 (left (right j))))
    (define f3 (run/bot* k3 (right (right j))))
    (λ: ([a : Value])
      (define b* (branch a))
      (define b (f1 a))
      (cond [(not (eq? b* b))  (bottom (delay (format "ifte*: expected ~a condition; got ~e" b* b)))]
            [(eq? b #t)  (f2 a)]
            [(eq? b #f)  (f3 a)]
            [else  (bottom (delay (format "ifte*: expected boolean condition; got ~e" b)))]))))

(: ifte*/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
#;; This direct translation from the paper ensures termination, but doesn't allow ifte*/pre* to rule
;; out branches using preimages computed by h1
(define (ifte*/pre* k1 k2 k3)
  (λ: ([j : Tree-Index])
    (define hb (run/pre* branch/pre* j))
    (define h1 (run/pre* k1 (left j)))
    (define h2 (run/pre* k2 (left (right j))))
    (define h3 (run/pre* k3 (right (right j))))
    (λ: ([A : Nonempty-Set])
      (let ([hb  (hb A)]
            [h1  (h1 A)])
        (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? hb))  empty-pre-mapping]
              [else
               (match-define (nonempty-pre-mapping C1 p1) h1)
               (match-define (nonempty-pre-mapping Cb pb) hb)
               (define C (set-intersect C1 Cb))
               (define Ct (set-intersect C trues))
               (define Cf (set-intersect C falses))
               (define A2 (if (empty-set? Ct) empty-set (set-intersect (p1 Ct) (pb Ct))))
               (define A3 (if (empty-set? Cf) empty-set (set-intersect (p1 Cf) (pb Cf))))
               (cond [(eq? Cb bools)
                      (define A (set-join A2 A3))
                      (nonempty-pre-mapping universe (λ (B) A))]
                     [else
                      (uplus/pre (run/pre h2 A2) (run/pre h3 A3))])])))))

;; This approximation *does* allow ifte*/pre* to rule out branches using preimages computed by h1,
;; but doesn't always ensure termination
;; Conjecture: if a program's interpretation as a bot* arrow terminates with probability 1, a pre*
;; arrow interpretation that uses this approximation also terminates with probability 1
(define (ifte*/pre* k1 k2 k3)
  (λ: ([j : Tree-Index])
    (define hb (run/pre* branch/pre* j))
    (define h1 (run/pre* k1 (left j)))
    (define h2 (run/pre* k2 (left (right j))))
    (define h3 (run/pre* k3 (right (right j))))
    (λ: ([A : Nonempty-Set])
      (let ([h1  (h1 A)])
        (define A2 (ap/pre h1 trues))
        (define A3 (ap/pre h1 falses))
        (define hb2 (run/pre hb A2))
        (define hb3 (run/pre hb A3))
        (let ([A2  (set-intersect A2 (ap/pre hb2 trues))]
              [A3  (set-intersect A3 (ap/pre hb3 falses))])
          (cond [(and (empty-set? A2) (empty-set? A3))  empty-pre-mapping]
                [(empty-set? A3)  (h2 A2)]
                [(empty-set? A2)  (h3 A3)]
                [else  (define A (set-join A2 A3))
                       (nonempty-pre-mapping universe (λ (B) A))]))))))

(: ifte*/idx (Idx-Arrow Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((ifte*/idx k1 k2 k3) j)
  (append (k1 (left j))
          (list (ifte*-index (reverse j)
                             (delay (k2 (left (right j))))
                             (delay (k3 (right (right j))))))))
