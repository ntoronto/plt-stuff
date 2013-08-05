#lang typed/racket

(require "set.rkt"
         "pre-mapping.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage arrow

(define-type (Pre-Arrow S1 S2) (S1 -> (pre-mapping S1 S2)))

(: >>>/pre (All (S1 X1 S2 X2 S3 X3)
                ((set S1 X1)
                 (set S2 X2)
                 (set S3 X3)
                 -> ((Pre-Arrow S1 S2) (Pre-Arrow S2 S3) -> (Pre-Arrow S1 S3)))))
(define (>>>/pre ops1 ops2 ops3)
  (define comp (pre-comp ops1 ops2 ops3))
  (λ (h1 h2)
    (λ (A)
      (let* ([h1  (h1 A)]
             [h2  (h2 (pre-range h1))])
        (comp h2 h1)))))

(: &&&/pre (All (S1 X1 S2 X2 S3 X3)
                ((set S1 X1)
                 (set S2 X2)
                 (set S3 X3)
                 -> ((Pre-Arrow S1 S2) (Pre-Arrow S1 S3) -> (Pre-Arrow S1 (Pair-Set S2 S3))))))
(define (&&&/pre ops1 ops2 ops3)
  (define pair (pre-pair ops1 ops2 ops3))
  (λ (h1 h2) (λ (A) (pair (h1 A) (h2 A)))))

(: if/pre (All (S1 X1 S2 X2)
               ((set S1 X1)
                (set S2 X2)
                -> ((Pre-Arrow S1 Bool-Set) (Pre-Arrow S1 S2) (Pre-Arrow S1 S2)
                                            -> (Pre-Arrow S1 S2)))))
(define (if/pre ops1 ops2)
  (define ap (pre-ap ops1 bool-set-ops))
  (define plus (pre-plus ops1 ops2))
  (λ (h1 h2 h3)
    (λ (A)
      (let ([h1  (h1 A)])
        (plus (h2 (ap h1 #t)) (h3 (ap h1 #f)))))))

(: lazy/pre (All (S1 X1 S2 X2)
                 ((set S1 X1) (set S2 X2) -> ((-> (Pre-Arrow S1 S2)) -> (Pre-Arrow S1 S2)))))
(define (lazy/pre ops1 ops2)
  (define empty? (set-empty? ops1))
  (define empty (empty-pre-mapping ops1 ops2))
  (λ (h) (λ (A) (if (empty? A) empty ((h) A)))))

(: id/pre (All (S X) ((set S X) -> (Pre-Arrow S S))))
(define ((id/pre ops) A)
  (pre-mapping A (λ: ([B : S]) B)))

(: const/pre (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (X2 -> (Pre-Arrow S1 S2)))))
(define (const/pre ops1 ops2)
  (define singleton (set-singleton ops2))
  (λ (a2) (λ (A) (pre-mapping (singleton a2) (λ: ([B : S2]) A)))))

(: fst/pre (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (Pre-Arrow (Pair-Set S1 S2) S1))))
(define (fst/pre ops1 ops2)
  (define proj-fst (set-proj-fst ops1 ops2))
  (define proj-snd (set-proj-snd ops1 ops2))
  (define meet (set-meet (pair-set-ops ops1 ops2)))
  (define prod (set-prod ops1 ops2))
  (λ (A)
    (define A1 (proj-fst A))
    (define A2 (proj-snd A))
    (pre-mapping A1 (λ: ([B : S1]) (meet A (prod B A2))))))

(: snd/pre (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (Pre-Arrow (Pair-Set S1 S2) S2))))
(define (snd/pre ops1 ops2)
  (define proj-fst (set-proj-fst ops1 ops2))
  (define proj-snd (set-proj-snd ops1 ops2))
  (define meet (set-meet (pair-set-ops ops1 ops2)))
  (define prod (set-prod ops1 ops2))
  (λ (A)
    (define A1 (proj-fst A))
    (define A2 (proj-snd A))
    (pre-mapping A2 (λ: ([B : S2]) (meet A (prod A1 B))))))


(: halt-on-true/pre (Pre-Arrow Bool-Set Bool-Set))
(define halt-on-true/pre
  ((if/pre bool-set-ops bool-set-ops)
   (id/pre bool-set-ops)
   (id/pre bool-set-ops)
   ((lazy/pre bool-set-ops bool-set-ops) (λ () halt-on-true/pre))))
