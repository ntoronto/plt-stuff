#lang typed/racket

(require "../types.rkt"
         "../branch-trace.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Bottom arrow

(define-type (Bot-Arrow X Y) (X -> (Maybe Y)))

(: arr/bot (All (X Y) ((X -> Y) -> (Bot-Arrow X Y))))
(define ((arr/bot f) x) (just (f x)))

(: >>>/bot (All (X Y Z) ((Bot-Arrow X Y) (Bot-Arrow Y Z) -> (Bot-Arrow X Z))))
(define ((>>>/bot f1 f2) x)
  (let ([y  (f1 x)])
    (if (bottom? y) bottom (f2 (just-value y)))))

(: pair/bot (All (X Y Z) ((Bot-Arrow X Y) (Bot-Arrow X Z) -> (Bot-Arrow X (Pair Y Z)))))
(define ((pair/bot f1 f2) x)
  (let ([y  (f1 x)])
    (if (bottom? y) y (let ([z  (f2 x)])
                        (if (bottom? z) z (just (cons (just-value y) (just-value z))))))))

(: lazy/bot (All (X Y) ((-> (Bot-Arrow X Y)) -> (Bot-Arrow X Y))))
(define ((lazy/bot f) x) ((f) x))

(: if/bot (All (X Y) ((Bot-Arrow X Boolean) (Bot-Arrow X Y) (Bot-Arrow X Y) -> (Bot-Arrow X Y))))
(define ((if/bot c t f) x)
  (define b (c x))
  (cond [(bottom? b)  bottom]
        [else  (let ([b  (just-value b)])
                 (if b (t x) (f x)))]))

(: id/bot (All (X) (Bot-Arrow X X)))
(define (id/bot x)
  (((inst arr/bot X X) (λ (x) x)) x))

(: const/bot (All (X Y) (Y -> (Bot-Arrow X Y))))
(define (const/bot y)
  ((inst arr/bot X Y) (λ (x) y)))

(: fst/bot (All (X Y) (Bot-Arrow (Pair X Y) X)))
(define (fst/bot xy)
  (((inst arr/bot (Pair X Y) X) car) xy))

(: snd/bot (All (X Y) (Bot-Arrow (Pair X Y) Y)))
(define (snd/bot xy)
  (((inst arr/bot (Pair X Y) Y) cdr) xy))

(: agrees/bot (Bot-Arrow (Pair Boolean Boolean) Boolean))
(define (agrees/bot xy)
  (if (equal? (car xy) (cdr xy)) (just (car xy)) bottom))

(: π/bot (Tree-Index -> (Bot-Arrow Branch-Trace Boolean)))
(define ((π/bot j) b)
  (define x ((π j) b))
  (if (bottom? x) bottom (just x)))
