#lang typed/racket/base

(require racket/match
         plot/typed
         "../main.rkt")

(provide (all-defined-out))

(require/typed
 profile
 [profile-thunk  ((-> Any) -> Void)])

(define: b : Boolean #f)

(define-syntax-rule (profile-expr e . args)
  (let* ([thnk  (λ () e)]
         [val  (if b (thnk) #f)])
    (profile-thunk (λ () (set! val (thnk))) . args)
    (assert val (λ: ([x : Any]) x))))

(: interval->ivl (Interval -> ivl))
(define (interval->ivl A)
  (match-define (interval a b a? b?) A)
  (ivl a b))

(: maybe-pad-list (All (A) ((Listof A) Integer (-> A) -> (Listof A))))
(define (maybe-pad-list lst n thnk)
  (append lst (build-list (max 0 (- n (length lst))) (λ (_) (thnk)))))

(: omega-rect->plot-rect (Omega-Rect -> (Listof ivl)))
(define (omega-rect->plot-rect Ω)
  (define rs (omega-rect-domain Ω))
  (define lst (map (λ: ([r : Omega-Idx]) (interval->ivl (omega-rect-ref Ω r))) rs))
  (maybe-pad-list lst 3 (λ () (ivl 0 1))))

(: omega->point (Omega -> (Listof Flonum)))
(define (omega->point ω)
  (define rs (omega-domain ω))
  (define lst (map (λ: ([r : Omega-Idx]) (omega-ref ω r)) rs))
  (maybe-pad-list lst 3 random))

(: value->listof-flonum (Value -> (Listof Flonum)))
(define (value->listof-flonum v)
  (cond [(flonum? v)  (list v)]
        [(boolean? v)  (list (if v (+ 0.9 (* 0.1 (random))) (* 0.1 (random))))]
        [(pair? v)  (append (value->listof-flonum (car v))
                            (value->listof-flonum (cdr v)))]
        [(null? v)  (list)]
        [else  (list -1.0)]))
