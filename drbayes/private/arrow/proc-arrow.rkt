#lang typed/racket/base

(require racket/promise
         racket/flonum
         racket/match
         "../set.rkt"
         "pure-arrows.rkt"
         "pure-lifts.rkt")

(provide (all-defined-out))

(define-type Proc-Arrow (Value -> Value))

(: lower/proc (Bot-Arrow -> Proc-Arrow))
(define ((lower/proc f) a)
  (define b (f a))
  (if (bottom? b) (error 'drbayes (force (bottom-message b))) b))

(: lift-R/proc (Symbol (Flonum -> Value) -> Proc-Arrow))
(define ((lift-R/proc name f) a)
  (cond [(flonum? a)  (f a)]
        [else  (error 'drbayes (format "~a: expected argument in reals; given ~e" name a))]))

(: lift-RxR/proc (Symbol (Flonum Flonum -> Value) -> Proc-Arrow))
(define ((lift-RxR/proc name f) a)
  (match a
    [(cons (? flonum? a1) (? flonum? a2))  (f a1 a2)]
    [_
     (error 'drbayes (format "~a: expected argument in (set-pair reals reals); given ~e" name a))]))

;; ===================================================================================================
;; Basic lifts

(define id/proc (λ: ([a : Value]) a))
(define const/proc (λ: ([b : Value]) (λ: ([a : Value]) b)))
(define fail/proc (lower/proc fail/bot))
(define restrict/proc (λ: ([X : Nonempty-Set]) (lower/proc (restrict/bot X))))
(define ref/proc (λ: ([j : Pair-Index]) (lower/proc (ref/bot j))))

;; ===================================================================================================
;; Combinators

(: >>>/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((>>>/proc f1 f2) a)
  (f2 (f1 a)))

(: &&&/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((&&&/proc f1 f2) a)
  (cons (f1 a) (f2 a)))

(: ifte/proc (Proc-Arrow Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((ifte/proc f1 f2 f3) a)
  (if (f1 a) (f2 a) (f3 a)))

(: lazy/proc ((-> Proc-Arrow) -> Proc-Arrow))
(define ((lazy/proc f) a) ((f) a))

(define ifte*/proc ifte/proc)

(: random/proc Proc-Arrow)
(define (random/proc a) (random))

(: boolean/proc (Flonum -> Proc-Arrow))
(define ((boolean/proc p) a) ((random) . < . p))

;; ===================================================================================================
;; Derived combinators

(define null/proc (const/proc null))

(: list/proc (Proc-Arrow * -> Proc-Arrow))
(define (list/proc . ks)
  (foldr &&&/proc null/proc ks))

(: apply/proc (Proc-Arrow (Listof Proc-Arrow) -> Proc-Arrow))
(define (apply/proc body args)
  ((list/proc (apply list/proc args)) . >>>/proc . body))

(: let/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define (let/proc expr body)
  ((expr . &&&/proc . id/proc) . >>>/proc . body))

;; ===================================================================================================
;; Other lifts

(define tag?/proc (λ: ([tag : Tag]) (lower/proc (tag?/bot tag))))
(define tag/proc (λ: ([tag : Tag]) (lower/proc (tag/bot tag))))
(define untag/proc (λ: ([tag : Tag]) (lower/proc (untag/bot tag))))

(define real?/proc real?)
(define null?/proc null?)
(define pair?/proc pair?)
(define boolean?/proc boolean?)

(define scale/proc (λ: ([y : Flonum]) (lift-R/proc 'scale (λ: ([x : Flonum]) (fl* x y)))))
(define translate/proc (λ: ([y : Flonum]) (lift-R/proc 'translate (λ: ([x : Flonum]) (fl+ x y)))))
(define neg/proc (lift-R/proc 'neg flneg))
(define exp/proc (lift-R/proc 'exp flexp))
(define log/proc (lower/proc log/bot))
(define sqrt/proc (lower/proc sqrt/bot))
(define asin/proc (lower/proc asin/bot))
(define acos/proc (lower/proc acos/bot))

(define cauchy/proc (lower/proc cauchy/bot))
(define normal/proc (lower/proc normal/bot))

(define +/proc (lift-RxR/proc '+ fl+))
(define -/proc (lift-RxR/proc '- fl-))

(define negative?/proc (lift-R/proc 'negative? negative?))
(define positive?/proc (lift-R/proc 'positive? positive?))
(define nonpositive?/proc (lower/proc nonpositive?/bot))
(define nonnegative?/proc (lower/proc nonnegative?/bot))

(define lt/proc (lift-RxR/proc '< fl<))
(define gt/proc (lift-RxR/proc '> fl>))
(define lte/proc (lift-RxR/proc '<= fl<=))
(define gte/proc (lift-RxR/proc '>= fl>=))

(define abs/proc (lift-R/proc 'abs flabs))
(define sqr/proc (lift-R/proc 'sqr (λ: ([x : Flonum]) (fl* x x))))
(define recip/proc (lower/proc recip/bot))

(define */proc (lift-RxR/proc '* fl*))
(define //proc (lower/proc //bot))

(define partial-cos/proc (lower/proc partial-cos/bot))
(define partial-sin/proc (lower/proc partial-sin/bot))
