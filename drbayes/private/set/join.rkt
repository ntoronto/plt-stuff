#lang typed/racket/base

(provide set-join)

(require racket/match
         "extremal-set.rkt"
         "interval.rkt"
         "null-rect.rkt"
         "boolean-rect.rkt"
         "union.rkt")

(: set-join (case-> (Empty-Set Empty-Set -> Empty-Set)
                    ((U Empty-Set Bot-Entry) Bot-Entry -> (U Bot-Entry Bot-Union))
                    ((U Empty-Set Bot-Entry) Top-Entry -> (U Universe Top-Entry Top-Union))
                    ((U Universe  Top-Entry) Top-Entry -> (U Universe Top-Entry Top-Union))
                    (Set Nonempty-Set -> Nonempty-Set)
                    (Nonempty-Set Set -> Nonempty-Set)
                    (Set Universe -> Universe)
                    (Universe Set -> Universe)
                    (Set Set -> Set)))
(define (set-join A B)
  ;(printf "A = ~v~nB = ~v~n~n" A B)
  (cond [(empty-set? A)  B]
        [(empty-set? B)  A]
        [(universe? A)   A]
        [(universe? B)   B]
        [(eq? A B)  B]
        [(bot-union? A)
         (cond [(or (bot-entry? B) (bot-union? B))  (bot-bot-join A B)]
               [(or (top-entry? B) (top-union? B))  (top-bot-join B A)])]
        [(top-union? A)
         (cond [(or (bot-entry? B) (bot-union? B))  (top-bot-join A B)]
               [(or (top-entry? B) (top-union? B))  (top-top-join A B)])]
        [(bot-union? B)
         (cond [(bot-entry? A)  (bot-bot-join A B)]
               [(top-entry? A)  (top-bot-join A B)])]
        [(top-union? B)
         (cond [(bot-entry? A)  (top-bot-join B A)]
               [(top-entry? A)  (top-top-join A B)])]
        [(rect? A)
         (cond [(rect? B)      (bot-bot-rect-join A B)]
               [(top-rect? B)  (top-bot-rect-join B A)]
               [(bot-set? B)   (bot-union A B)]
               [(top-set? B)   B])]
        [(top-rect? A)
         (cond [(rect? B)      (top-bot-rect-join A B)]
               [(top-rect? B)  (top-top-rect-join A B)]
               [(bot-set? B)   A]
               [(top-set? B)   universe])]
        [(bot-set? A)
         (cond [(rect? B)      (bot-union A B)]
               [(top-rect? B)  B]
               [(bot-set? B)   (bot-bot-set-join A B)]
               [(top-set? B)   (top-bot-set-join B A)])]
        [(top-set? A)
         (cond [(rect? B)      A]
               [(top-rect? B)  universe]
               [(bot-set? B)   (top-bot-set-join A B)]
               [(top-set? B)   (top-top-set-join A B)])]))

(: bot-bot-join ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union) -> (U Bot-Entry Bot-Union)))
(define (bot-bot-join A B)
  (for/fold ([A A]) ([Bsub  (in-list (bot-union-sets B))])
    (define Asub (bot-union-ref A (bot-tag Bsub)))
    (bot-union-add A (set-join Asub Bsub))))

(: top-bot-join ((U Top-Entry Top-Union) (U Bot-Entry Bot-Union) -> (U Universe Top-Entry Top-Union)))
(define (top-bot-join A B)
  (for/fold: ([A : (U Universe Top-Entry Top-Union)  A]) ([Asub  (in-list (top-union-sets A))])
    (define tag (get-tag Asub))
    (define Bsub (bot-union-ref B tag))
    (define C (set-join Bsub Asub))
    (if (universe? C) (top-union-remove A tag) (top-union-add A C))))

(: top-top-join ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> (U Universe Top-Entry Top-Union)))
(define (top-top-join A B)
  (for/fold: ([C : (U Universe Top-Entry Top-Union)  universe]) ([Bsub  (in-list (top-union-sets B))])
    (define tag (get-tag Bsub))
    (define Asub (top-union-ref A tag))
    (define D (set-join Asub Bsub))
    (if (universe? D) C (top-union-add C D))))

(: pair-rect-join (Pair-Rect Pair-Rect -> Pair-Rect))
(define (pair-rect-join A1×A2 B1×B2)
  (match-define (pair-rect A1 A2) A1×A2)
  (match-define (pair-rect B1 B2) B1×B2)
  (define C1 (set-join A1 B1))
  (define C2 (set-join A2 B2))
  (define A1? (eq? C1 A1))
  (define A2? (eq? C2 A2))
  (if (and A1? A2?)
      A1×A2
      (let ([B1?  (eq? C1 B1)]
            [B2?  (eq? C2 B2)])
        (if (and B1? B2?)
            B1×B2
            (pair-rect (if A1? A1 (if B1? B1 C1))
                       (if A2? A2 (if B2? B2 C2)))))))

(: rect-join (case-> (Rect Rect -> (U #t Rect))
                     (Maybe-Rect Rect -> (U #t Rect))
                     (Maybe-Rect Maybe-Rect -> (U #t Maybe-Rect))))
(define (rect-join A B)
  (cond [(empty-set? A)  B]
        [(empty-set? B)  A]
        [(and (interval? A) (interval? B))  (interval-join A B)]
        [(and (null-rect? A) (null-rect? B))  A]
        [(and (pair-rect? A) (pair-rect? B))  (pair-rect-join A B)]
        [(and (boolean-rect? A) (boolean-rect? B))  (boolean-rect-join A B)]
        [else  #t]))

(: bot-bot-rect-join (Rect Rect -> (U Rect Bot-Union)))
(define (bot-bot-rect-join A B)
  (define C (rect-join A B))
  (cond [(eq? C #t)  (bot-union A B)]
        [(eq? C A)  A]
        [(eq? C B)  B]
        [else  C]))

(: top-bot-rect-join (Top-Rect Rect -> (U Universe Top-Rect)))
(define (top-bot-rect-join A B)
  (match-define (Tagged a-tag Asub) A)
  (define Csub (if (eq? a-tag (rect-tag B)) (rect-join Asub B) #t))
  (cond [(eq? Csub #t)  A]
        [(eq? Csub Asub)  A]
        [else  (top-rect a-tag Csub)]))

(: top-top-rect-join (Top-Rect Top-Rect -> (U Universe Top-Rect)))
(define (top-top-rect-join A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (rect-join Asub Bsub) #t))
  (cond [(eq? Csub #t)  universe]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-rect a-tag Csub)]))

(: bot-bot-set-join (Bot-Set Bot-Set -> (U Bot-Set Bot-Union)))
(define (bot-bot-set-join A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (set-join Asub Bsub) #t))
  (cond [(eq? Csub #t)  (bot-union A B)]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (bot-set a-tag Csub)]))

(: top-bot-set-join (Top-Set Bot-Set -> (U Universe Top-Set)))
(define (top-bot-set-join A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (set-join Asub Bsub) #t))
  (cond [(eq? Csub #t)  A]
        [(eq? Csub Asub)  A]
        [else  (top-set a-tag Csub)]))

(: top-top-set-join (Top-Set Top-Set -> (U Universe Top-Set)))
(define (top-top-set-join A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (define Csub (if (eq? a-tag b-tag) (set-join Asub Bsub) #t))
  (cond [(eq? Csub #t)  universe]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-set a-tag Csub)]))
