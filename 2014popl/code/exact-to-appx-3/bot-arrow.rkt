#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Function arrow

(define-type (Bot-Arrow X Y) (X -> (U Y Bottom)))

(: arr/bot (All (X Y) ((X -> Y) -> (Bot-Arrow X Y))))
(define ((arr/bot f) x) (f x))

(: >>>/bot (All (X Y Z) ((Bot-Arrow X Y) (Bot-Arrow Y Z) -> (Bot-Arrow X Z))))
(define ((>>>/bot f1 f2) x)
  (let ([y  (f1 x)])
    (if (bottom? y) bottom (f2 y))))

(: pair/bot (All (X Y Z) ((Bot-Arrow X Y) (Bot-Arrow X Z) -> (Bot-Arrow X (Pair Y Z)))))
(define ((pair/bot f1 f2) x)
  (let ([y  (f1 x)])
    (if (bottom? y) y (let ([z  (f2 x)])
                        (if (bottom? z) z (cons y z))))))

(: if/bot (All (X Y) ((Bot-Arrow X Boolean) (-> (Bot-Arrow X Y)) (-> (Bot-Arrow X Y))
                                            -> (Bot-Arrow X Y))))
(define ((if/bot c t f) x)
  (define b (c x))
  (cond [(eq? b #t)  ((t) x)]
        [(eq? b #f)  ((f) x)]
        [else  bottom]))

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

;; ===================================================================================================
;; Other combinators (play area)

;; From Yallop et al
; app : (X ~> Y)×X ~> Y
(: app/bot (All (X Y) (Bot-Arrow (Pair (Bot-Arrow X Y) X) Y)))
;(: app/bot (All (X Y) ((Pair (X -> (U Y Bottom)) X) -> (U Y Bottom))))
(define (app/bot fx)
  ((car fx) (cdr fx)))

;; Defining (strict) conditional in terms of other combinators

(: switch (All (X) ((Pair Boolean (Pair X X)) -> X)))
(define (switch x)
  (if (car x) (car (cdr x)) (cdr (cdr x))))

(: switch/bot (All (X Y) ((Bot-Arrow X Y) (Bot-Arrow X Y) -> (Bot-Arrow (Pair Boolean X) Y))))
(define (switch/bot t f)
  (>>>/bot (pair/bot (inst fst/bot Boolean X)
                     (pair/bot (>>>/bot (inst snd/bot Boolean X) t)
                               (>>>/bot (inst snd/bot Boolean X) f)))
           ((inst arr/bot (Pair Boolean (Pair Y Y)) Y) switch)))

(: iff/bot (All (X Y) ((Bot-Arrow X Boolean) (Bot-Arrow X Y) (Bot-Arrow X Y) -> (Bot-Arrow X Y))))
(define (iff/bot c t f)
  (>>>/bot (pair/bot c (inst id/bot X)) ((inst switch/bot X Y) t f)))

