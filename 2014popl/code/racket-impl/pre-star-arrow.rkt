#lang typed/racket

(require "set.rkt"
         "tree-set.rkt"
         "pre-mapping.rkt"
         "pre-arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage arrow

(define-type Maybe-Bool-Set (Maybe-Set Bool-Set))
(define maybe-bool-set-ops (maybe-set-ops bool-set-ops))

(define-type RSet (Tree-Set Real-Set))
(define-type TSet (Tree-Set (Maybe-Set Bool-Set)))

(define-type (Pre*-Arrow S1 S2) (Tree-Index -> (Pre-Arrow (Pair-Set (Pair-Set RSet TSet) S1) S2)))

(define full-rset-axis (interval 0.0 1.0))
(define full-tset-axis (with-bot univ-bool-set))

(define rset-ops (tree-set-ops real-set-ops full-rset-axis))
(define tset-ops (tree-set-ops maybe-bool-set-ops full-tset-axis))
(define store-set-ops (pair-set-ops rset-ops tset-ops))

(: >>>/pre* (All (S1 X1 S2 X2 S3 X3)
                 ((set S1 X1)
                  (set S2 X2)
                  (set S3 X3)
                  -> ((Pre*-Arrow S1 S2) (Pre*-Arrow S2 S3) -> (Pre*-Arrow S1 S3)))))
(define (>>>/pre* ops1 ops2 ops3)
  (define fst (fst/pre store-set-ops ops1))
  (define &&& (&&&/pre (pair-set-ops store-set-ops ops1) store-set-ops ops2))
  (define >>> (>>>/pre (pair-set-ops store-set-ops ops1) (pair-set-ops store-set-ops ops2) ops3))
  (λ (k1 k2)
    (λ (j) ((fst . &&& . (k1 (index-left j))) . >>> . (k2 (index-right j))))))

(: &&&/pre* (All (S1 X1 S2 X2 S3 X3)
                 ((set S1 X1)
                  (set S2 X2)
                  (set S3 X3)
                  -> ((Pre*-Arrow S1 S2) (Pre*-Arrow S1 S3) -> (Pre*-Arrow S1 (Pair-Set S2 S3))))))
(define (&&&/pre* ops1 ops2 ops3)
  (define &&& (&&&/pre (pair-set-ops store-set-ops ops1) ops2 ops3))
  (λ (k1 k2)
    (λ (j) ((k1 (index-left j)) . &&& . (k2 (index-right j))))))

(: if/pre* (All (S S1 X1 S2 X2)
                ((set S1 X1)
                 (set S2 X2)
                 -> ((Pre*-Arrow S1 Bool-Set) (Pre*-Arrow S1 S2) (Pre*-Arrow S1 S2)
                                              -> (Pre*-Arrow S1 S2)))))
(define (if/pre* ops1 ops2)
  (define ifte (if/pre (pair-set-ops store-set-ops ops1) ops2))
  (λ (k1 k2 k3)
    (λ (j) (ifte (k1 (index-left j))
                 (k2 (index-left (index-right j)))
                 (k3 (index-right (index-right j)))))))

(: lazy/pre* (All (S1 X1 S2 X2)
                  ((set S1 X1) (set S2 X2) -> ((-> (Pre*-Arrow S1 S2)) -> (Pre*-Arrow S1 S2)))))
(define (lazy/pre* ops1 ops2)
  (define lazy (lazy/pre (pair-set-ops store-set-ops ops1) ops2))
  (λ (k)
    (λ (j) (lazy (λ () ((k) j))))))


(: lift/pre* (All (S1 X1 S2 X2)
                  ((set S1 X1) (set S2 X2) -> ((Pre-Arrow S1 S2) -> (Pre*-Arrow S1 S2)))))
(define (lift/pre* ops1 ops2)
  (define snd (snd/pre store-set-ops ops1))
  (define >>> (>>>/pre (pair-set-ops store-set-ops ops1) ops1 ops2))
  (λ (h)
    (λ (j) (snd . >>> . h))))

(: id/pre* (All (S X) ((set S X) -> (Pre*-Arrow S S))))
(define (id/pre* ops) ((lift/pre* ops ops) (id/pre ops)))

(: const/pre* (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (X2 -> (Pre*-Arrow S1 S2)))))
(define ((const/pre* ops1 ops2) a2)
  ((lift/pre* ops1 ops2) ((const/pre ops1 ops2) a2)))

(: fst/pre* (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (Pre*-Arrow (Pair-Set S1 S2) S1))))
(define (fst/pre* ops1 ops2)
  ((lift/pre* (pair-set-ops ops1 ops2) ops1) (fst/pre ops1 ops2)))

(: snd/pre* (All (S1 X1 S2 X2) ((set S1 X1) (set S2 X2) -> (Pre*-Arrow (Pair-Set S1 S2) S2))))
(define (snd/pre* ops1 ops2)
  ((lift/pre* (pair-set-ops ops1 ops2) ops2) (snd/pre ops1 ops2)))

;; ---------------------------------------------------------------------------------------------------
;; Random numbers

(define project-real (project real-set-ops full-rset-axis))
(define unproject-real (unproject real-set-ops full-rset-axis))

(: random/pre (Tree-Index -> (Pre-Arrow RSet Real-Set)))
(define ((random/pre j) A)
  (pre-mapping (project-real j A)
               (λ: ([B : Real-Set]) (unproject-real j A B))))

(: random/pre* (All (S X) ((set S X) -> (Pre*-Arrow S Real-Set))))
(define (random/pre* ops)
  (define fst0 (fst/pre store-set-ops ops))
  (define >>>0 (>>>/pre (pair-set-ops store-set-ops ops) store-set-ops real-set-ops))
  (define fst1 (fst/pre rset-ops tset-ops))
  (define >>>1 (>>>/pre store-set-ops rset-ops real-set-ops))
  (λ (j)
    (fst0 . >>>0 . (fst1 . >>>1 . (random/pre j)))))

;; ---------------------------------------------------------------------------------------------------
;; Conditional that always converges

(define project-bool (project maybe-bool-set-ops full-tset-axis))
(define unproject-bool (unproject maybe-bool-set-ops full-tset-axis))

(: branch/pre (Tree-Index -> (Pre-Arrow TSet Bool-Set)))
(define ((branch/pre j) A)
  (pre-mapping (without-bot (project-bool j A))
               (λ: ([B : Bool-Set]) (unproject-bool j A (only-just B)))))

(: branch/pre* (All (S X) ((set S X) -> (Pre*-Arrow S Bool-Set))))
(define (branch/pre* ops)
  (define fst0 (fst/pre store-set-ops ops))
  (define >>>0 (>>>/pre (pair-set-ops store-set-ops ops) store-set-ops bool-set-ops))
  (define snd1 (snd/pre rset-ops tset-ops))
  (define >>>1 (>>>/pre store-set-ops tset-ops bool-set-ops))
  (λ (j)
    (fst0 . >>>0 . (snd1 . >>>1 . (branch/pre j)))))

(: convif/pre* (All (S S1 X1 S2 X2)
                    ((set S1 X1)
                     (set S2 X2)
                     -> ((Pre*-Arrow S1 Bool-Set) (Pre*-Arrow S1 S2) (Pre*-Arrow S1 S2)
                                                  -> (Pre*-Arrow S1 S2)))))
(define (convif/pre* ops1 ops2)
  (define branch1 (branch/pre* ops1))
  (define ops (pair-set-ops store-set-ops ops1))
  (define meet (set-meet ops))
  (define join (set-join ops))
  (define empty? (set-empty? ops))
  (define empty-pre (empty-pre-mapping ops ops2))
  (define univ2 (set-univ ops2))
  (define plus (pre-plus ops ops2))
  (λ (k1 k2 k3)
    (λ (j)
      (λ (A)
        (match-define (pre-mapping Ck pk) ((k1 (index-left j)) A))
        (match-define (pre-mapping Cb pb) ((branch1 j) A))
        (define C (bool-set-meet Ck Cb))
        (define C2 (bool-set-meet C #t))
        (define C3 (bool-set-meet C #f))
        (define A2 (meet (pk C2) (pb C2)))
        (define A3 (meet (pk C3) (pb C3)))
        (cond [(univ-bool-set? Cb)
               (pre-mapping univ2 (λ: ([B : S2]) (join A2 A3)))]
              [else
               (plus ((k2 (index-left (index-right j))) A2)
                     ((k3 (index-right (index-right j))) A3))])))))

;; ===================================================================================================

(: halt-on-true/pre* (Pre*-Arrow Bool-Set Bool-Set))
(define halt-on-true/pre*
  ((convif/pre* bool-set-ops bool-set-ops)
   (id/pre* bool-set-ops)
   (id/pre* bool-set-ops)
   ((lazy/pre* bool-set-ops bool-set-ops) (λ () halt-on-true/pre*))))

((pre-ap (pair-set-ops store-set-ops bool-set-ops) bool-set-ops)
 ((halt-on-true/pre* j0) univ-pair-set) 
 #t)

((pre-ap (pair-set-ops store-set-ops bool-set-ops) bool-set-ops)
 ((halt-on-true/pre* j0) univ-pair-set)
 #f)

((pre-ap (pair-set-ops store-set-ops bool-set-ops) bool-set-ops)
 ((halt-on-true/pre* j0) ((set-prod store-set-ops bool-set-ops)
                          ((set-prod rset-ops tset-ops)
                           univ-tree-set
                           (unproject-bool '() univ-tree-set (only-just #f)))
                          univ-bool-set))
 univ-bool-set)
