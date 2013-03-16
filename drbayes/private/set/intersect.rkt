#lang typed/racket/base

(provide set-intersect)

(require racket/match
         racket/list
         "extremal-set.rkt"
         "interval.rkt"
         "null-rect.rkt"
         "boolean-rect.rkt"
         "union.rkt")

(: set-intersect (case-> (Universe Universe -> Universe)
                         ((U Universe Top-Entry) Top-Entry -> (U Top-Entry Top-Union))
                         ((U Universe Top-Entry) Bot-Entry -> (U Empty-Set Bot-Entry Bot-Union))
                         ((U Empty-Set Bot-Entry) Bot-Entry -> (U Empty-Set Bot-Entry Bot-Union))
                         (Set Nonfull-Set -> Nonfull-Set)
                         (Nonfull-Set Set -> Nonfull-Set)
                         (Set Empty-Set -> Empty-Set)
                         (Empty-Set Set -> Empty-Set)
                         (Set Set -> Set)))
(define (set-intersect A B)
  (cond [(empty-set? A)  A]
        [(empty-set? B)  B]
        [(universe? A)   B]
        [(universe? B)   A]
        [(eq? A B)  B]
        [(bot-union? A)  (if (set-nonfull-top? B) (bot-top-intersect A B) (bot-bot-intersect A B))]
        [(top-union? A)  (if (set-nonfull-top? B) (top-top-intersect A B) (bot-top-intersect B A))]
        [(bot-union? B)  (if (top-entry? A) (bot-top-intersect B A) (bot-bot-intersect A B))]
        [(top-union? B)  (if (top-entry? A) (top-top-intersect A B) (bot-top-intersect A B))]
        [(interval? A)
         (cond [(interval? B)  (interval-intersect A B)]
               [(top-rect? B)  (bot-top-rect-intersect A B)]
               [(top-set? B)   A]
               [else           empty-set])]
        [(null-rect? A)
         (cond [(null-rect? B)  A]
               [(top-rect? B)   (bot-top-rect-intersect A B)]
               [(top-set? B)    A]
               [else            empty-set])]
        [(pair-rect? A)
         (cond [(pair-rect? B)  (pair-rect-intersect A B)]
               [(top-rect? B)   (bot-top-rect-intersect A B)]
               [(top-set? B)    A]
               [else            empty-set])]
        [(boolean-rect? A)
         (cond [(boolean-rect? B)  (boolean-rect-intersect A B)]
               [(top-rect? B)      (bot-top-rect-intersect A B)]
               [(top-set? B)       A]
               [else               empty-set])]
        [(top-rect? A)
         (cond [(top-rect? B)  (top-top-rect-intersect A B)]
               [(bot-set? B)   B]
               [(top-set? B)   (top-union A B)]
               [else           (bot-top-rect-intersect B A)])]
        [(bot-set? A)
         (cond [(top-rect? B)  A]
               [(bot-set? B)   (bot-bot-set-intersect A B)]
               [(top-set? B)   (bot-top-set-intersect A B)]
               [else           empty-set])]
        [(top-set? A)
         (cond [(top-rect? B)  (top-union A B)]
               [(bot-set? B)   (bot-top-set-intersect B A)]
               [(top-set? B)   (top-top-set-intersect A B)]
               [else           B])]
        ))

(: bot-bot-intersect ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                                              -> (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-bot-intersect A B)
  (for/fold: ([C : (U Empty-Set Bot-Entry Bot-Union)  empty-set]
              ) ([Bsub  (in-list (bot-union-sets B))])
    (define tag (bot-tag Bsub))
    (define Asub (bot-union-ref A tag))
    (define D (set-intersect Asub Bsub))
    (if (empty-set? D) C (bot-union-add C D))))

(: bot-top-intersect ((U Bot-Entry Bot-Union) (U Top-Entry Top-Union)
                                              -> (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-top-intersect A B)
  (for/fold: ([A : (U Empty-Set Bot-Entry Bot-Union)  A]) ([Asub  (in-list (bot-union-sets A))])
    (define tag (bot-tag Asub))
    (define Bsub (top-union-ref B tag))
    (define C (set-intersect Bsub Asub))
    (if (empty-set? C) (bot-union-remove A tag) (bot-union-add A C))))

(: top-top-intersect ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> (U Top-Entry Top-Union)))
(define (top-top-intersect A B)
  (for/fold ([A A]) ([Bsub  (in-list (top-union-sets B))])
    (define Asub (top-union-ref A (get-tag Bsub)))
    (top-union-add A (set-intersect Asub Bsub))))

(: pair-rect-intersect (Pair-Rect Pair-Rect -> Maybe-Pair-Rect))
(define (pair-rect-intersect A1×A2 B1×B2)
  (define A1 (pair-rect-fst A1×A2))
  (define A2 (pair-rect-snd A1×A2))
  (define B1 (pair-rect-fst B1×B2))
  (define B2 (pair-rect-snd B1×B2))
  (define C1 (set-intersect A1 B1))
  (define C2 (set-intersect A2 B2))
  (if (or (empty-set? C1) (empty-set? C2))
      empty-set
      (let ()
        (define A1? (eq? C1 A1))
        (define A2? (eq? C2 A2))
        (if (and A1? A2?)
            A1×A2
            (let ([B1?  (eq? C1 B1)]
                  [B2?  (eq? C2 B2)])
              (if (and B1? B2?)
                  B1×B2
                  (pair-rect (if A1? A1 (if B1? B1 C1))
                             (if A2? A2 (if B2? B2 C2)))))))))

(: rect-intersect (Maybe-Rect Maybe-Rect -> (U #t Maybe-Rect)))
(define (rect-intersect A B)
  (cond [(empty-set? A)  A]
        [(empty-set? B)  B]
        [(and (interval? A) (interval? B))  (interval-intersect A B)]
        [(and (null-rect? A) (null-rect? B))  A]
        [(and (pair-rect? A) (pair-rect? B))  (pair-rect-intersect A B)]
        [(and (boolean-rect? A) (boolean-rect? B))  (boolean-rect-intersect A B)]
        [else  #t]))

(: bot-bot-rect-intersect (Rect Rect -> (U Empty-Set Rect)))
(define (bot-bot-rect-intersect A B)
  (define C (rect-intersect A B))
  (if (eq? C #t) empty-set C))

(: bot-top-rect-intersect (Rect Top-Rect -> (U Empty-Set Rect)))
(define (bot-top-rect-intersect A B)
  (match-define (Tagged b-tag Bsub) B)
  (define C (if (eq? (rect-tag A) b-tag) (rect-intersect A Bsub) #t))
  (if (eq? C #t) A C))

(: top-top-rect-intersect (Top-Rect Top-Rect -> (U Top-Rect Top-Union)))
(define (top-top-rect-intersect A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (rect-intersect Asub Bsub) #t))
  (cond [(eq? Csub #t)  (top-union A B)]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-rect a-tag Csub)]))

(: bot-bot-set-intersect (Bot-Set Bot-Set -> (U Empty-Set Bot-Set)))
(define (bot-bot-set-intersect A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (set-intersect Asub Bsub) #t))
  (cond [(eq? Csub #t)  empty-set]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (bot-set a-tag Csub)]))

(: bot-top-set-intersect (Bot-Set Top-Set -> (U Empty-Set Bot-Set)))
(define (bot-top-set-intersect A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (set-intersect Asub Bsub) #t))
  (cond [(eq? Csub #t)  A]
        [(eq? Csub Asub)  A]
        [else  (bot-set a-tag Csub)]))

(: top-top-set-intersect (Top-Set Top-Set -> (U Top-Set Top-Union)))
(define (top-top-set-intersect A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (set-intersect Asub Bsub) #t))
  (cond [(eq? Csub #t)  (top-union A B)]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-set a-tag Csub)]))
