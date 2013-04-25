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
(define ((prim-preimage-fun->rand pre) Z K) (values omega-rect branches-rect (pre K)))

(: prim-computation->rand (Prim-Computation -> Rand-Computation))
(define ((prim-computation->rand comp) _Ω _Z Γ)
  (define meaning (comp Γ))
  (cond [(empty-meaning? meaning)  empty-meaning]
        [else  (match-define (prim-computation-meaning K pre) meaning)
               (rand-computation-meaning branches-rect K (prim-preimage-fun->rand pre))]))

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
                     (rand-switch/arr (->rand t-expr) (->rand f-expr))))

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

(: boolean/pre (Omega-Rect Nonempty-Set Maybe-Interval* Maybe-Interval* -> Rand-Preimage-Fun))
(define (boolean/pre Ω Γ It If)
  (define Ω1 (omega-rect-fst Ω))
  (define Ω2 (omega-rect-snd Ω))
  (λ (Z K)
    (define I (cond [(eq? K booleans)  (interval*-union It If)]
                    [(eq? K trues)     It]
                    [(eq? K falses)    If]
                    [else              empty-set]))
    (cond [(interval*? I)  (values (omega-rect-node I Ω1 Ω2) Z Γ)]
          [else            (values empty-set empty-set empty-set)])))

(: boolean/comp (Interval Interval -> Rand-Computation))
(define (boolean/comp It If)
  (cached-rand-computation
   (λ (Ω Z Γ)
     (define I (omega-rect-value Ω))
     (let ([It  (interval*-intersect I It)]
           [If  (interval*-intersect I If)])
       (define K (booleans->boolean-rect (not (empty-set? It)) (not (empty-set? If))))
       (cond [(empty-set? K)  empty-meaning]
             [else  (define pre (rand-preimage Ω Γ Z K (boolean/pre Ω Γ It If)))
                    (rand-computation-meaning Z K pre)])))))

(: boolean/arr (Flonum -> Expression))
(define (boolean/arr p)
  (cond
    [(and (p . > . 0.0) (p . < . 1.0))
     (define It (Interval 0.0 p #t #f))
     (define If (Interval p 1.0 #t #t))
     (define split (make-constant-splitter (list It If)))
     (rand-expression
      (λ (r)
        (define idx (reverse r))
        (rand-expression-meaning (list (interval-index idx split))
                                 (boolean/fwd idx p)
                                 (boolean/comp It If))))]
    [(= p 0.0)  (c/arr #f)]
    [(= p 1.0)  (c/arr #t)]
    [else  (raise-argument-error 'boolean "probability" p)]))
