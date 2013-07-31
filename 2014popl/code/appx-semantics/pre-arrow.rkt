#lang typed/racket

(require "../branch-trace.rkt"
         "set.rkt"
         "preimage-mapping.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage arrow

(define-type (Pre-Arrow S1 S2) (S1 -> (U Empty-Set (pmapping S1 S2))))

(: >>>/pre (All (S1 X1 S2 X2 S3 X3)
                ((set-ops S1 X1)
                 (set-ops S2 X2)
                 (set-ops S3 X3)
                 -> ((Pre-Arrow S1 S2) (Pre-Arrow S2 S3) -> (Pre-Arrow S1 S3)))))
(define (((>>>/pre ops1 ops2 ops3) h1 h2) A)
  (let ([h1  (h1 A)])
    (cond [(∅? h1)  ∅]
          [else  (let ([h2  (h2 (pmapping-range h1))])
                   ((pmapping-compose ops1 ops2 ops3) h2 h1))])))

(: &&&/pre (All (S1 X1 S2 X2 S3 X3)
                 ((set-ops S1 X1)
                  (set-ops S2 X2)
                  (set-ops S3 X3)
                  -> ((Pre-Arrow S1 S2) (Pre-Arrow S1 S3) -> (Pre-Arrow S1 (set-prod S2 S3))))))
(define (((&&&/pre ops1 ops2 ops3) h1 h2) A)
  ((pmapping-pair ops1 ops2 ops3) (h1 A) (h2 A)))

(: lazy/pre (All (S1 S2) ((-> (Pre-Arrow S1 S2)) -> (Pre-Arrow S1 S2))))
(define ((lazy/pre h) A)
  (cond [(∅? A)  ∅]
        [else  ((h) A)]))

(: if/pre (All (S S1 X1 S2 X2)
               ((set-ops S Boolean)
                (set-ops S1 X1)
                (set-ops S2 X2)
                -> ((Pre-Arrow S1 S) (Pre-Arrow S1 S2) (Pre-Arrow S1 S2) -> (Pre-Arrow S1 S2)))))
(define (((if/pre bops ops1 ops2) c t f) A)
  (let ([c  (c A)])
    (cond [(∅? c)  ∅]
          [else  (define At ((pmapping-ap ops1 bops) c ((set-singleton bops) #t)))
                 (define Af ((pmapping-ap ops1 bops) c ((set-singleton bops) #f)))
                 ((pmapping-union ops1 ops2) (if (∅? At) ∅ (t At))
                                             (if (∅? Af) ∅ (f Af)))])))


(: id/pre (All (S X) ((set-ops S X) -> (Pre-Arrow S S))))
(define ((id/pre ops) A)
  (pmapping A (λ: ([B : S]) B)))

(: const/pre (All (S1 X1 S2 X2)
                  ((set-ops S1 X1)
                   (set-ops S2 X2)
                   -> (X2 -> (Pre-Arrow S1 S2)))))
(define (((const/pre ops1 ops2) y) A)
  (pmapping ((set-singleton ops2) y) (λ: ([B : S2]) A)))

(: fst/pre (All (S1 X1 S2 X2)
                ((set-ops (set-prod S1 S2) (Pair X1 X2)) -> (Pre-Arrow (set-prod S1 S2) S1))))
(define ((fst/pre ops) A)
  (match-define (set-prod A1 A2) A)
  (pmapping A1 (λ: ([B : S1]) ((set-meet ops) A (set-prod B A2)))))

(: snd/pre (All (S1 X1 S2 X2)
                ((set-ops (set-prod S1 S2) (Pair X1 X2)) -> (Pre-Arrow (set-prod S1 S2) S2))))
(define ((snd/pre ops) A)
  (match-define (set-prod A1 A2) A)
  (pmapping A2 (λ: ([B : S2]) ((set-meet ops) A (set-prod A1 B)))))

#|
(: agrees/pre (Pre-Arrow (Pair Boolean Boolean) Boolean))
(define agrees/pre
  (lift/pre agrees/map))

(: π/pre (Tree-Index -> (Pre-Arrow Branch-Trace Boolean)))
(define (π/pre j)
  (lift/pre (π/map j)))
|#

(: pair-id/pre (All (S1 X1 S2 X2)
                    ((set-ops S1 X1)
                     (set-ops S2 X2)
                     -> (Pre-Arrow (set-prod S1 S2) (set-prod S1 S2)))))
(define (pair-id/pre ops1 ops2)
  (define ops (set-ops-prod ops1 ops2))
  ((fst/pre ops) . (&&&/pre ops ops1 ops2) . (snd/pre ops)))

(define RxR-ops (set-ops-prod ivl-ops ivl-ops))

((pmapping-ap RxR-ops RxR-ops)
 ((pair-id/pre ivl-ops ivl-ops)
  (set-prod (interval 0.0 1.0) (interval 0.5 1.5)))
 (set-prod (interval 0.5 1.5) (interval 0.0 1.0)))
