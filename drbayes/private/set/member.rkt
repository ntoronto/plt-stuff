#lang typed/racket/base

(provide set-member?)

(require racket/match
         "extremal-set.rkt"
         "interval.rkt"
         "null-rect.rkt"
         "boolean-rect.rkt"
         "union.rkt"
         "value.rkt")

(: set-member? (case-> (Empty-Set Value -> #f)
                       (Universe Value -> #t)
                       (Set Value -> Boolean)))
(define (set-member? A x)
  (cond [(empty-set? A)  #f]
        [(universe? A)   #t]
        [(rect? A)       (bot-rect-member? A x)]
        [(top-rect? A)   (top-rect-member? A x)]
        [(bot-set? A)    (bot-set-member? A x)]
        [(top-set? A)    (top-set-member? A x)]
        [(bot-union? A)  (bot-union-member? A x)]
        [(top-union? A)  (top-union-member? A x)]))

(: pair-rect-member? (Pair-Rect (Pair Value Value) -> Boolean))
(define (pair-rect-member? A1×A2 x1x2)
  (match-define (pair-rect A1 A2) A1×A2)
  (and (set-member? A1 (car x1x2))
       (set-member? A2 (cdr x1x2))))

(: rect-member? (Maybe-Rect Value -> Boolean))
(define (rect-member? A x)
  (cond [(empty-set? A)  #f]
        [(interval? A)   (and (flonum? x) (interval-member? A x))]
        [(null-rect? A)  (null? x)]
        [(pair-rect? A)  (and (pair? x) (pair-rect-member? A x))]
        [(boolean-rect? A)  (and (boolean? x) (boolean-rect-member? A x))]))

(: bot-rect-member? (Rect Value -> Boolean))
(define (bot-rect-member? A x)
  (rect-member? A x))

(: top-rect-member? (Top-Rect Value -> Boolean))
(define (top-rect-member? A x)
  (or (not (eq? (get-tag A) (value-tag x)))
      (rect-member? (get-val A) x)))

(: bot-set-member? (Bot-Set Value -> Boolean))
(define (bot-set-member? A x)
  (and (tagged? x)
       (eq? (get-tag A) (get-tag x))
       (set-member? (get-val A) (get-val x))))

(: top-set-member? (Top-Set Value -> Boolean))
(define (top-set-member? A x)
  (or (not (tagged? x))
      (not (eq? (get-tag A) (get-tag x)))
      (set-member? (get-val A) (get-val x))))

(: bot-union-member? (Bot-Union Value -> Boolean))
(define (bot-union-member? A x)
  (set-member? (bot-union-ref A (value-tag x)) x))

(: top-union-member? (Top-Union Value -> Boolean))
(define (top-union-member? A x)
  (set-member? (top-union-ref A (value-tag x)) x))
