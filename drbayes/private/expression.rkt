#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "types.rkt"
         "rect.rkt"
         "arrow.rkt")

(provide (all-defined-out))

(: apply/arr (expression (Listof expression) -> expression))
(define (apply/arr body args)
  (ap/arr body (list/arr (apply list/arr args))))

;; ---------------------------------------------------------------------------------------------------
;; Primitives

(define-syntax-rule (define-wrapped-unary/arr name f/arr)
  (begin (: name (expression -> expression))
         (define (name x) (ap/arr f/arr x))))

(define-syntax-rule (define-wrapped-binary/arr name f/arr)
  (begin (: name (expression expression -> expression))
         (define (name x y) (ap/arr f/arr (pair/arr x y)))))

(define-wrapped-unary/arr neg/exp neg/arr)
(define-wrapped-unary/arr inv/exp inv/arr)
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
(define (scale/exp x c) (ap/arr (scale/arr c) x))

(: translate/exp (expression Flonum -> expression))
(define (translate/exp x c) (ap/arr (translate/arr c) x))

;; ---------------------------------------------------------------------------------------------------
;; Pairs and lists

(define pair/exp pair/arr)
(define null/exp null/arr)
(define list/exp list/arr)

(: fst/exp (expression -> expression))
(define (fst/exp p)
  (ap/arr (ref/arr 'fst) p))

(: snd/exp (expression -> expression))
(define (snd/exp p)
  (ap/arr (ref/arr 'snd) p))

(: list-ref/exp (expression Idx -> expression))
(define (list-ref/exp lst j)
  (ap/arr (ref/arr j) lst))

;; ---------------------------------------------------------------------------------------------------
;; Environment

(: let/exp (expression expression -> expression))
(define (let/exp expr body)
  (rap/arr (pair/arr expr id/arr) body))

(define env/exp ref/arr)

;; ---------------------------------------------------------------------------------------------------
;; Random store

(define random/exp random/arr)
(define boolean/exp random-boolean/arr)

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
    [()  (ap/arr normal/arr (uniform/exp))]
    [(m)  (+/exp m (normal/exp))]
    [(m s)  (+/exp m (*/exp (normal/exp) s))]))

(: cauchy/exp (case-> (-> expression)
                      (expression -> expression)
                      (expression expression -> expression)))
(define cauchy/exp
  (case-lambda
    [()  (ap/arr cauchy/arr (uniform/exp))]
    [(m)  (+/exp m (cauchy/exp))]
    [(m s)  (+/exp m (*/exp (cauchy/exp) s))]))

;; ---------------------------------------------------------------------------------------------------
;; if

(: delay/exp ((-> expression) -> expression))
(define (delay/exp e)
  (let ([e  (delay (e))])
    (expression (λ (r0 r1) (run-expression (force e) r0 r1)))))

(: lazy-if/exp (expression (-> expression) (-> expression) -> expression))
(define (lazy-if/exp c t f)
  (lazy-if/arr c (delay/exp t) (delay/exp f)))

(: strict-if/exp (expression (-> expression) (-> expression) -> expression))
(define (strict-if/exp c t f)
  (strict-if/arr c (delay/exp t) (delay/exp f)))
