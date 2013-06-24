#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Function arrow

(define-type (Fun-Arrow X Y) (X -> (U Y Bottom)))

(: fun-arr (All (X Y) ((X -> Y) -> (Fun-Arrow X Y))))
(define ((fun-arr f) x) (f x))

(: fun->>> (All (X Y Z) ((Fun-Arrow X Y) (Fun-Arrow Y Z) -> (Fun-Arrow X Z))))
(define ((fun->>> f1 f2) x)
  (let ([y  (f1 x)])
    (if (bottom? y) bottom (f2 y))))

(: fun-pair (All (X Y Z) ((Fun-Arrow X Y) (Fun-Arrow X Z) -> (Fun-Arrow X (Pair Y Z)))))
(define ((fun-pair f1 f2) x)
  (let ([y  (f1 x)]
        [z  (f2 x)])
    (if (or (bottom? y) (bottom? z)) bottom (cons y z))))

(: fun-if (All (X Y) ((Fun-Arrow X Boolean) (-> (Fun-Arrow X Y)) (-> (Fun-Arrow X Y))
                                            -> (Fun-Arrow X Y))))
(define ((fun-if c t f) x)
  (define b (c x))
  (cond [(eq? b #t)  ((t) x)]
        [(eq? b #f)  ((f) x)]
        [else  bottom]))

(: fun-id (All (X) (Fun-Arrow X X)))
(define (fun-id x)
  (((inst fun-arr X X) (λ (x) x)) x))

(: fun-const (All (X Y) (Y -> (Fun-Arrow X Y))))
(define (fun-const y)
  ((inst fun-arr X Y) (λ (x) y)))

(: fun-fst (All (X Y) (Fun-Arrow (Pair X Y) X)))
(define (fun-fst xy)
  (((inst fun-arr (Pair X Y) X) car) xy))

(: fun-snd (All (X Y) (Fun-Arrow (Pair X Y) Y)))
(define (fun-snd xy)
  (((inst fun-arr (Pair X Y) Y) cdr) xy))

(: fun-bottom (All (X) (Fun-Arrow X Nothing)))
(define (fun-bottom x)
  (((inst fun-const X Bottom) bottom) x))

; app : (X ~> Y)×X ~> Y

(: fun-app (All (X Y) (Fun-Arrow (Pair (Fun-Arrow X Y) X) Y)))
;(: fun-app (All (X Y) ((Pair (X -> (U Y Bottom)) X) -> (U Y Bottom))))
(define (fun-app fx)
  ((car fx) (cdr fx)))

#|
(: fun-lazy (All (X Y) ((-> (Fun-Arrow X Y)) -> (Fun-Arrow X Y))))
(define ((fun-lazy f) x) ((f) x))
|#

;; ===================================================================================================

(: fun-halt-on-true (Fun-Arrow Boolean Boolean))
(define fun-halt-on-true
  (fun-if (inst fun-id Boolean)
          (λ () ((inst fun-const Boolean Boolean) #t))
          (λ () fun-halt-on-true)))

(fun-halt-on-true #t)
;; Diverges:
;(fun-halt-on-true (set #f))
