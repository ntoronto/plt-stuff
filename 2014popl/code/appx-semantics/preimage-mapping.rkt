#lang typed/racket

(require "set.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage mappings

(struct: (S1 S2) pmapping ([range : S2]
                           [fun : (S2 -> (U Empty-Set S1))])
  #:transparent)

(: pmapping-ap (All (S1 X1 S2 X2)
                    ((set-ops S1 X1)
                     (set-ops S2 X2)
                     -> ((U Empty-Set (pmapping S1 S2)) S2 -> (U Empty-Set S1)))))
(define ((pmapping-ap ops1 ops2) h B)
  (cond [(∅? h)  ∅]
        [else  (match-define (pmapping Y p) h)
               (let ([B  ((set-meet ops2) B Y)])
                 (if (∅? B) ∅ (p B)))]))

(: pmapping-pair (All (S1 X1 S2 X2 S3 X3)
                      ((set-ops S1 X1)
                       (set-ops S2 X2)
                       (set-ops S3 X3)
                       -> ((U Empty-Set (pmapping S1 S2))
                           (U Empty-Set (pmapping S1 S3))
                           -> (U Empty-Set (pmapping S1 (set-prod S2 S3)))))))
(define ((pmapping-pair ops1 ops2 ops3) h1 h2)
  (cond [(or (∅? h1) (∅? h2))  ∅]
        [else  (match-define (pmapping Y1 p1) h1)
               (match-define (pmapping Y2 p2) h2)
               (define Y (set-prod (pmapping-range h1) (pmapping-range h2)))
               (define p (λ: ([B : (set-prod S2 S3)])
                           (define A1 (p1 (set-prod-fst B)))
                           (cond [(∅? A1)  ∅]
                                 [else  (define A2 (p2 (set-prod-snd B)))
                                        (cond [(∅? A2)  ∅]
                                              [else  ((set-meet ops1) A1 A2)])])))
               (pmapping Y p)]))

(: pmapping-compose (All (S1 X1 S2 X2 S3 X3)
                         ((set-ops S1 X1)
                          (set-ops S2 X2)
                          (set-ops S3 X3)
                          -> ((U Empty-Set (pmapping S2 S3))
                              (U Empty-Set (pmapping S1 S2))
                              -> (U Empty-Set (pmapping S1 S3))))))
(define ((pmapping-compose ops1 ops2 ops3) h2 h1)
  (cond [(or (∅? h1) (∅? h2))  ∅]
        [else  (match-define (pmapping Z p2) h2)
               (pmapping Z (λ: ([C : S3])
                             (define B (p2 C))
                             (cond [(∅? B)  ∅]
                                   [else  ((pmapping-ap ops1 ops2) h1 B)])))]))

(: pmapping-union (All (S1 X1 S2 X2)
                       ((set-ops S1 X1)
                        (set-ops S2 X2)
                        -> ((U Empty-Set (pmapping S1 S2))
                            (U Empty-Set (pmapping S1 S2))
                            -> (U Empty-Set (pmapping S1 S2))))))
(define ((pmapping-union ops1 ops2) h1 h2)
  (cond [(∅? h1)  h2]
        [(∅? h2)  h1]
        [else  (define Y ((set-join ops2) (pmapping-range h1) (pmapping-range h2)))
               (define p (λ: ([B : S2])
                           (define A1 ((pmapping-ap ops1 ops2) h1 B))
                           (define A2 ((pmapping-ap ops1 ops2) h2 B))
                           (cond [(∅? A1)  A2]
                                 [(∅? A2)  A1]
                                 [else  ((set-join ops1) A1 A2)])))
               (pmapping Y p)]))
