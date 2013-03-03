#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "omega.rkt"
         "rect.rkt"
         "arrow.rkt"
         "arrow-prims.rkt")

(provide (all-defined-out))

(: apply/exp (expression (Listof expression) -> expression))
(define (apply/exp body args)
  (rcompose/arr (list/arr (apply list/arr args)) body))

;; ---------------------------------------------------------------------------------------------------
;; Primitives

(define-syntax-rule (define-wrapped-unary/arr name f/arr)
  (begin (: name (expression -> expression))
         (define (name x) (rcompose/arr x f/arr))))

(define-syntax-rule (define-wrapped-binary/arr name f/arr)
  (begin (: name (expression expression -> expression))
         (define (name x y) (rcompose/arr (pair/arr x y) f/arr))))

(define-wrapped-unary/arr neg/exp neg/arr)
(define-wrapped-unary/arr recip/exp recip/arr)
(define-wrapped-unary/arr exp/exp exp/arr)
(define-wrapped-unary/arr log/exp log/arr)
(define-wrapped-unary/arr sqrt/exp sqrt/arr)
(define-wrapped-unary/arr sqr/exp sqr/arr)
(define-wrapped-unary/arr negative?/exp negative?/arr)
(define-wrapped-unary/arr nonpositive?/exp nonpositive?/arr)
(define-wrapped-unary/arr positive?/exp positive?/arr)
(define-wrapped-unary/arr nonnegative?/exp nonnegative?/arr)
(define-wrapped-unary/arr null?/exp null?/arr)
(define-wrapped-binary/arr +/exp +/arr)
(define-wrapped-binary/arr -/exp -/arr)
(define-wrapped-binary/arr */exp */arr)
(define-wrapped-binary/arr //exp //arr)
(define-wrapped-binary/arr lt/exp lt/arr)
(define-wrapped-binary/arr lte/exp lte/arr)
(define-wrapped-binary/arr gt/exp gt/arr)
(define-wrapped-binary/arr gte/exp gte/arr)

(: scale/exp (expression Flonum -> expression))
(define (scale/exp x c) (rcompose/arr x (scale/arr c)))

(: translate/exp (expression Flonum -> expression))
(define (translate/exp x c) (rcompose/arr x (translate/arr c)))

;; ---------------------------------------------------------------------------------------------------
;; Pairs and lists

(define pair/exp pair/arr)
(define null/exp null/arr)
(define list/exp list/arr)

(: fst/exp (expression -> expression))
(define (fst/exp p)
  (rcompose/arr p (ref/arr 'fst)))

(: snd/exp (expression -> expression))
(define (snd/exp p)
  (rcompose/arr p (ref/arr 'snd)))

(: list-ref/exp (expression Idx -> expression))
(define (list-ref/exp lst j)
  (rcompose/arr lst (ref/arr j)))

;; ---------------------------------------------------------------------------------------------------
;; Environment

(: let/exp (expression expression -> expression))
(define (let/exp expr body)
  (rcompose/arr (pair/arr expr id/arr) body))

(define env/exp ref/arr)

;; ---------------------------------------------------------------------------------------------------
;; Random store

(define random/exp random/arr)
(define boolean/exp boolean/arr)

(: uniform/exp (case-> (-> expression)
                       (expression -> expression)
                       (expression expression -> expression)))
(define uniform/exp
  (case-lambda
    [()  random/exp]
    [(b)  (*/exp b random/exp)]
    [(a b)  (+/exp a (*/exp (-/exp b a) random/exp))]))

(: normal/exp (case-> (-> expression)
                      (expression -> expression)
                      (expression expression -> expression)))
(define normal/exp
  (case-lambda
    [()  (rcompose/arr (uniform/exp) normal/arr)]
    [(m)  (+/exp m (normal/exp))]
    [(m s)  (+/exp m (*/exp (normal/exp) s))]))

(: cauchy/exp (case-> (-> expression)
                      (expression -> expression)
                      (expression expression -> expression)))
(define cauchy/exp
  (case-lambda
    [()  (rcompose/arr (uniform/exp) cauchy/arr)]
    [(m)  (+/exp m (cauchy/exp))]
    [(m s)  (+/exp m (*/exp (cauchy/exp) s))]))

;; ---------------------------------------------------------------------------------------------------
;; if

(: delay/exp ((-> expression) -> expression))
(define (delay/exp e)
  (let ([e  (delay (e))])
    (expression (λ (r) (run-expression (force e) r)))))

(: lazy-if/exp (expression (-> expression) (-> expression) -> expression))
(define (lazy-if/exp c t f)
  (lazy-if/arr c (delay/exp t) (delay/exp f)))

(: strict-if/exp (expression (-> expression) (-> expression) -> expression))
(define (strict-if/exp c t f)
  (strict-if/arr c (delay/exp t) (delay/exp f)))
