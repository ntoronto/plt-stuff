#lang typed/racket/base

(provide (all-defined-out))

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "../untyped-utils.rkt"
         "indexes.rkt"
         "arrow-common.rkt"
         "prim-arrow.rkt"
         "rand-arrow.rkt")

(: prim-forward-fun->rand (Prim-Forward-Fun -> Rand-Forward-Fun))
(define ((prim-forward-fun->rand f) ω z γ) (f γ))

(: prim-preimage-fun->rand (Prim-Preimage-Fun -> Rand-Preimage-Fun))
(define ((prim-preimage-fun->rand pre) Γ K)
  (match-define (nonempty-domain-set Ωin  Zin  Ain)  Γ)
  (match-define (nonempty-domain-set Ωout Zout Aout) K)
  (domain-set (omega-set-intersect Ωin Ωout)
              (branches-set-intersect Zin Zout)
              (pre Ain Aout)))

(: prim-computation->rand (Prim-Computation -> Rand-Computation))
(define ((prim-computation->rand comp) Γ)
  (match-define (nonempty-domain-set Ω Z A) Γ)
  (match-define (prim-preimage Ain Aout f) (comp A))
  (rand-preimage (domain-set Ω Z Ain)
                 (domain-set Ω Z Aout)
                 (prim-preimage-fun->rand f)))

(: prim-expression->rand (prim-expression -> rand-expression))
(define (prim-expression->rand e)
  (rand-expression
   (λ (r)
     (match-define (prim-expression-meaning fwd pre) (run-prim-expression e))
     (rand-expression-meaning empty
                              (prim-forward-fun->rand fwd)
                              (prim-computation->rand pre)))))

(define-type Expression (U prim-expression rand-expression))

(define expression?
  (λ: ([e : Any]) (or (prim-expression? e) (rand-expression? e))))

(: ->rand (Expression -> rand-expression))
(define (->rand e)
  (cond [(rand-expression? e)  e]
        [else  (prim-expression->rand e)]))

;; ===================================================================================================
;; Primitiveness-preserving combinators

(: rcompose/arr (case-> (prim-expression prim-expression -> prim-expression)
                        (Expression Expression -> Expression)))
(define (rcompose/arr f g)
  (cond [(and (prim-expression? f) (prim-expression? g))  (prim-rcompose/arr f g)]
        [else  (rand-rcompose/arr (->rand f) (->rand g))]))

(: pair/arr (case-> (prim-expression prim-expression -> prim-expression)
                    (Expression Expression -> Expression)))
(define (pair/arr e1 e2)
  (cond [(and (prim-expression? e1) (prim-expression? e2))  (prim-pair/arr e1 e2)]
        [else  (rand-pair/arr (->rand e1) (->rand e2))]))

(: lazy-if/arr (Expression Expression Expression -> Expression))
(define (lazy-if/arr c-expr t-expr f-expr)
  (rand-rcompose/arr (->rand (pair/arr c-expr id/arr))
                     (rand-switch/arr (->rand (rcompose/arr (ref/arr 'snd) t-expr))
                                      (->rand (rcompose/arr (ref/arr 'snd) f-expr)))))

(: strict-if/arr (case-> (prim-expression prim-expression prim-expression -> prim-expression)
                         (Expression Expression Expression -> Expression)))
(define (strict-if/arr c t f)
  (cond [(and (prim-expression? c) (prim-expression? t) (prim-expression? f))
         (prim-if/arr c t f)]
        [else
         (rcompose/arr (pair/arr c (pair/arr t f))
                       (prim-if/arr (ref/arr 'fst)
                                    (prim-rcompose/arr (ref/arr 'snd) (ref/arr 'fst))
                                    (prim-rcompose/arr (ref/arr 'snd) (ref/arr 'snd))))]))

;; ===================================================================================================
;; Random boolean

(: boolean/fwd (Omega-Index Flonum -> Rand-Forward-Fun))
(define ((boolean/fwd r p) ω z γ)
  ((omega-ref ω r) . < . p))

(: boolean/pre (Omega-Index Nonfull-Real-Set Nonfull-Real-Set -> Rand-Preimage-Fun))
(define ((boolean/pre idx It If) Γ K)
  (match-define (nonempty-domain-set Ωin  Zin  Ain)  Γ)
  (match-define (nonempty-domain-set Ωout Zout Aout) K)
  (define t? (set-member? Aout #t))
  (define f? (set-member? Aout #f))
  (define I (cond [(and t? f?)  (real-set-intersect unit-interval (real-set-union It If))]
                  [t?  It]
                  [f?  If]
                  [else  empty-real-set]))
  (cond [(empty-real-set? I)  empty-set]
        [else
         (domain-set (omega-set-intersect Ωin (omega-set-restrict Ωout idx I))
                     (branches-set-intersect Zin Zout)
                     Ain)]))

(: boolean/comp (Omega-Index Nonextremal-Interval Nonextremal-Interval -> Rand-Computation))
(define ((boolean/comp idx It If) Γ)
  (match-define (nonempty-domain-set Ωin Zin Ain) Γ)
  (define I (omega-set-ref Ωin idx))
  (let ([It  (real-set-intersect I It)]
        [If  (real-set-intersect I If)])
    (define Aout (booleans->bool-set (not (empty-real-set? It))
                                     (not (empty-real-set? If))))
    (define K (cond [(empty-bool-set? Aout)  empty-set]
                    [else  (domain-set Ωin Zin Aout)]))
    (rand-preimage Γ K (boolean/pre idx It If))))

(: boolean/arr (Flonum -> Expression))
(define (boolean/arr p)
  (cond
    [(and (p . > . 0.0) (p . < . 1.0))
     (define It (Nonextremal-Interval 0.0 p #t #f))
     (define If (Nonextremal-Interval p 1.0 #t #t))
     (define split (make-constant-splitter (list It If)))
     (rand-expression
      (λ (r)
        (define idx (reverse r))
        (rand-expression-meaning (list (interval-index idx split))
                                 (boolean/fwd idx p)
                                 (boolean/comp idx It If))))]
    [(= p 0.0)  (c/arr #f)]
    [(= p 1.0)  (c/arr #t)]
    [else  (raise-argument-error 'boolean "probability" p)]))
