#lang typed/racket

(require "set.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage mappings

(struct: (S1 S2) pre-mapping ([range : S2] [fun : (S2 -> S1)]) #:transparent)

(define pre-range pre-mapping-range)

(: empty-pre-mapping (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (pre-mapping S1 S2))))
(define (empty-pre-mapping ops1 ops2)
  (pre-mapping (set-empty ops2) (λ (_) (set-empty ops1))))

(: pre-ap (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> ((pre-mapping S1 S2) S2 -> S1))))
(define (pre-ap ops1 ops2)
  (define meet2 (set-meet ops2)) 
  (λ (h B)
    (match-define (pre-mapping Y p) h)
    (p (meet2 B Y))))

(: pre-pair (All (S1 X1 S2 X2 S3 X3)
                 ((set S1 X1)
                  (set S2 X2)
                  (set S3 X3)
                  -> ((pre-mapping S1 S2) (pre-mapping S1 S3) -> (pre-mapping S1 (Pair-Set S2 S3))))))
(define (pre-pair ops1 ops2 ops3)
  (define prod (set-prod ops2 ops3))
  (define proj-fst (set-proj-fst ops2 ops3))
  (define proj-snd (set-proj-snd ops2 ops3))
  (define meet1 (set-meet ops1))
  (λ (h1 h2)
    (match-define (pre-mapping Y1 p1) h1)
    (match-define (pre-mapping Y2 p2) h2)
    (pre-mapping (prod Y1 Y2)
                 (λ: ([B : (Pair-Set S2 S3)])
                   (meet1 (p1 (proj-fst B)) (p2 (proj-snd B)))))))

(: pre-comp (All (S1 X1 S2 X2 S3 X3)
                 ((set S1 X1)
                  (set S2 X2)
                  (set S3 X3)
                  -> ((pre-mapping S2 S3) (pre-mapping S1 S2) -> (pre-mapping S1 S3)))))
(define (pre-comp ops1 ops2 ops3)
  (define ap (pre-ap ops1 ops2))
  (λ (h2 h1)
    (match-define (pre-mapping Z p2) h2)
    (pre-mapping Z (λ: ([C : S3]) (ap h1 (p2 C))))))

(: pre-plus (All (S1 X1 S2 X2)
                 ((set S1 X1)
                  (set S2 X2)
                  -> ((pre-mapping S1 S2) (pre-mapping S1 S2) -> (pre-mapping S1 S2)))))
(define (pre-plus ops1 ops2)
  (define join1 (set-join ops1))
  (define join2 (set-join ops2))
  (define ap (pre-ap ops1 ops2))
  (λ (h1 h2)
    (pre-mapping (join2 (pre-range h1) (pre-range h2))
                 (λ: ([B : S2]) (join1 (ap h1 B) (ap h2 B))))))
