#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "arrow-common.rkt"
         "prim-arrow.rkt"
         "rand-arrow.rkt"
         "arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; First-order function application

(: apply/exp (case-> (prim-expression (Listof prim-expression) -> prim-expression)
                     (Expression (Listof Expression) -> Expression)))
(define (apply/exp body args)
  (rcompose/arr (list/arr (apply list/arr args)) body))

;; ===================================================================================================
;; Lists

(define null/arr (c/arr null))

(: list/arr (case-> (prim-expression * -> prim-expression)
                    (Expression * -> Expression)))
(define (list/arr . es)
  (foldr pair/arr null/arr es))

;; ===================================================================================================
;; Primitives

(define-syntax-rule (define-wrapped-unary/arr name f/arr)
  (begin (: name (case-> (prim-expression -> prim-expression)
                         (Expression -> Expression)))
         (define (name x) (rcompose/arr x f/arr))))

(define-syntax-rule (define-wrapped-binary/arr name f/arr)
  (begin (: name (case-> (prim-expression prim-expression -> prim-expression)
                         (Expression Expression -> Expression)))
         (define (name x y) (rcompose/arr (pair/arr x y) f/arr))))

(define-wrapped-unary/arr real?/exp real?/arr)
(define-wrapped-unary/arr null?/exp null?/arr)
(define-wrapped-unary/arr pair?/exp pair?/arr)
(define-wrapped-unary/arr boolean?/exp boolean?/arr)

(define-wrapped-unary/arr neg/exp neg/arr)
(define-wrapped-unary/arr recip/exp recip/arr)
(define-wrapped-unary/arr exp/exp exp/arr)
(define-wrapped-unary/arr log/exp log/arr)
(define-wrapped-unary/arr abs/exp abs/arr)
(define-wrapped-unary/arr sqr/exp sqr/arr)
(define-wrapped-unary/arr sqrt/exp sqrt/arr)
(define-wrapped-unary/arr negative?/exp negative?/arr)
(define-wrapped-unary/arr nonpositive?/exp nonpositive?/arr)
(define-wrapped-unary/arr positive?/exp positive?/arr)
(define-wrapped-unary/arr nonnegative?/exp nonnegative?/arr)
(define-wrapped-unary/arr acos/exp acos/arr)
(define-wrapped-unary/arr asin/exp asin/arr)
(define-wrapped-unary/arr partial-cos/exp partial-cos/arr)
(define-wrapped-unary/arr partial-sin/exp partial-sin/arr)

(define-wrapped-binary/arr +/exp +/arr)
(define-wrapped-binary/arr -/exp -/arr)
(define-wrapped-binary/arr */exp */arr)
(define-wrapped-binary/arr //exp //arr)
(define-wrapped-binary/arr lt/exp lt/arr)
(define-wrapped-binary/arr lte/exp lte/arr)
(define-wrapped-binary/arr gt/exp gt/arr)
(define-wrapped-binary/arr gte/exp gte/arr)

(: scale/exp (case-> (prim-expression Flonum -> prim-expression)
                     (Expression Flonum -> Expression)))
(define (scale/exp x c) (rcompose/arr x (scale/arr c)))

(: translate/exp (case-> (prim-expression Flonum -> prim-expression)
                         (Expression Flonum -> Expression)))
(define (translate/exp x c) (rcompose/arr x (translate/arr c)))

;; ===================================================================================================
;; Pairs and lists

(define pair/exp pair/arr)
(define null/exp null/arr)
(define list/exp list/arr)

(: fst/exp (case-> (prim-expression -> prim-expression)
                   (Expression -> Expression)))
(define (fst/exp p)
  (rcompose/arr p (ref/arr 'fst)))

(: snd/exp (case-> (prim-expression -> prim-expression)
                   (Expression -> Expression)))
(define (snd/exp p)
  (rcompose/arr p (ref/arr 'snd)))

(: list-ref/exp (case-> (prim-expression Pair-Index -> prim-expression)
                        (Expression Pair-Index -> Expression)))
(define (list-ref/exp lst j)
  (rcompose/arr lst (ref/arr j)))

;; ===================================================================================================
;; Environment

(: let/exp (case-> (prim-expression prim-expression -> prim-expression)
                   (Expression Expression -> Expression)))
(define (let/exp expr body)
  (rcompose/arr (pair/arr expr id/arr) body))

(define env/exp ref/arr)

;; ===================================================================================================
;; Random store

(define random/exp random/arr)
(define boolean/exp boolean/arr)

(: uniform/exp (case-> (-> Expression)
                       (Expression -> Expression)
                       (Expression Expression -> Expression)))
(define uniform/exp
  (case-lambda
    [()  random/exp]
    [(b)  (*/exp b random/exp)]
    [(a b)  (+/exp a (*/exp (-/exp b a) random/exp))]))

(: normal/exp (case-> (-> Expression)
                      (Expression -> Expression)
                      (Expression Expression -> Expression)))
(define normal/exp
  (case-lambda
    [()  (rcompose/arr (uniform/exp) normal/arr)]
    [(m)  (+/exp m (normal/exp))]
    [(m s)  (+/exp m (*/exp (normal/exp) s))]))

(: cauchy/exp (case-> (-> Expression)
                      (Expression -> Expression)
                      (Expression Expression -> Expression)))
(define cauchy/exp
  (case-lambda
    [()  (rcompose/arr (uniform/exp) cauchy/arr)]
    [(m)  (+/exp m (cauchy/exp))]
    [(m s)  (+/exp m (*/exp (cauchy/exp) s))]))

;; ===================================================================================================
;; Conditionals

(: delay/exp (case-> ((-> Expression) -> rand-expression)))
(define (delay/exp e)
  (let ([e  (delay (->rand (e)))])
    (rand-expression (λ (r) (run-rand-expression (force e) r)))))

(: lazy-if/exp (Expression (-> Expression) (-> Expression) -> Expression))
(define (lazy-if/exp c t f)
  (lazy-if/arr c (delay/exp t) (delay/exp f)))

(: strict-if/exp
   (case-> (prim-expression (-> prim-expression) (-> prim-expression) -> prim-expression)
           (Expression (-> Expression) (-> Expression) -> Expression)))
(define (strict-if/exp c t f)
  (strict-if/arr c (t) (f)))

;; ===================================================================================================
;; Tagged sets

(: tag?/exp (case-> (prim-expression Symbol -> prim-expression)
                    (Expression Symbol -> Expression)))
(define (tag?/exp e tag)
  (rcompose/arr e (tag?/arr tag)))

(: tag/exp (case-> (prim-expression Symbol -> prim-expression)
                   (Expression Symbol -> Expression)))
(define (tag/exp e tag)
  (rcompose/arr e (tag/arr tag)))

(: untag/exp (case-> (prim-expression Symbol -> prim-expression)
                     (Expression Symbol -> Expression)))
(define (untag/exp e tag)
  (rcompose/arr e (untag/arr tag)))
