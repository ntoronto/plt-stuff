#lang typed/racket/base

(require (prefix-in : "../set-ops.rkt")
         (prefix-in : racket/set)
         racket/match)

(provide (all-defined-out))

(define ∅ '∅)
(define-type Empty-Set '∅)
(define ∅? (λ (A) (equal? A ∅)))

(define ⊤ '⊤)
(define-type Universe '⊤)
(define ⊤? (λ (A) (equal? A ⊤)))

(struct: (S X) set-ops ([meet : (S S -> (U Empty-Set S))]
                        [join : (S S -> S)]
                        [singleton : (X -> S)]
                        [member? : (S X -> Boolean)]))

(define set-meet set-ops-meet)
(define set-join set-ops-join)
(define set-member? set-ops-member?)
(define set-singleton set-ops-singleton)

(struct: interval ([min : Flonum] [max : Flonum])
  #:transparent)

(: ivl (Flonum Flonum -> (U Empty-Set interval)))
(define (ivl min max)
  (if (min . <= . max) (interval min max) ∅))

(: ivl-ops (set-ops interval Flonum))
(define ivl-ops
  (set-ops (λ: ([A : interval] [B : interval])
             (match-define (interval amin amax) A)
             (match-define (interval bmin bmax) B)
             (ivl (max amin bmin) (min amax bmax)))
           (λ: ([A : interval] [B : interval])
             (match-define (interval amin amax) A)
             (match-define (interval bmin bmax) B)
             (interval (min amin bmin) (max amax bmax)))
           (λ: ([a : Flonum]) (interval a a))
           (λ: ([A : interval] [a : Flonum])
             (match-define (interval amin amax) A)
             (<= amin a amax))))

(define-type Bool-Set (U 'tf #t #f))

(: bool-ops (set-ops Bool-Set Boolean))
(define bool-ops
  (set-ops (λ: ([A : Bool-Set] [B : Bool-Set])
             (cond [(eq? A 'tf)  B]
                   [(eq? B 'tf)  A]
                   [(eq? A B)  A]
                   [else  ∅]))
           (λ: ([A : Bool-Set] [B : Bool-Set])
             (cond [(eq? A 'tf)  A]
                   [(eq? B 'tf)  B]
                   [(eq? A B)  A]
                   [else  'tf]))
           (λ: ([a : Boolean]) a)
           (λ: ([A : Bool-Set] [a : Boolean])
             (cond [(eq? A 'tf)  #t]
                   [else  (eq? A a)]))))

(struct: (S1 S2) set-prod ([fst : S1] [snd : S2]) #:transparent)

(: set-ops-prod (All (S1 X1 S2 X2) ((set-ops S1 X1) (set-ops S2 X2)
                                                    -> (set-ops (set-prod S1 S2) (Pair X1 X2)))))
(define (set-ops-prod ops1 ops2)
  (set-ops (λ: ([A : (set-prod S1 S2)] [B : (set-prod S1 S2)])
             (define C1 ((set-ops-meet ops1) (set-prod-fst A) (set-prod-fst B)))
             (cond [(∅? C1)  ∅]
                   [else  (define C2 ((set-ops-meet ops2) (set-prod-snd A) (set-prod-snd B)))
                          (cond [(∅? C2)  ∅]
                                [else  (set-prod C1 C2)])]))
           (λ: ([A : (set-prod S1 S2)] [B : (set-prod S1 S2)])
             (set-prod ((set-ops-join ops1) (set-prod-fst A) (set-prod-fst B))
                       ((set-ops-join ops2) (set-prod-snd A) (set-prod-snd B))))
           (λ: ([a : (Pair X1 X2)])
             (set-prod ((set-ops-singleton ops1) (car a))
                       ((set-ops-singleton ops2) (cdr a))))
           (λ: ([A : (set-prod S1 S2)] [a : (Pair X1 X2)])
             (and ((set-ops-member? ops1) (set-prod-fst A) (car a))
                  ((set-ops-member? ops2) (set-prod-snd A) (cdr a))))))
                                                     
(: set-ops-universal (All (S X) ((set-ops S X) -> (set-ops (U S Universe) X))))
(define (set-ops-universal ops)
  (set-ops (λ: ([A : (U S Universe)] [B : (U S Universe)])
             (cond [(⊤? A)  B]
                   [(⊤? B)  A]
                   [else  ((set-meet ops) A B)]))
           (λ: ([A : (U S Universe)] [B : (U S Universe)])
             (cond [(⊤? A)  A]
                   [(⊤? B)  B]
                   [else  ((set-join ops) A B)]))
           (set-singleton ops)
           (λ: ([A : (U S Universe)] [a : X])
             (if (⊤? A) #t ((set-member? ops) A a)))))
