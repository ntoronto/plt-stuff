#lang typed/racket/base

(provide set-subseteq?)

(require racket/match
         "extremal-set.rkt"
         "interval.rkt"
         "null-rect.rkt"
         "boolean-rect.rkt"
         "union.rkt")

(: set-subseteq? (Set Set -> Boolean))
(define (set-subseteq? A B)
  ;(printf "A = ~v~nB = ~v~n~n" A B)
  (cond [(empty-set? A)  #t]
        [(empty-set? B)  #f]
        [(universe? B)   #t]
        [(universe? A)   #f]
        [(eq? A B)  #t]
        [(rect? A)
         (cond [(rect? B)       (bot-bot-rect-subseteq? A B)]
               [(top-rect? B)   (bot-top-rect-subseteq? A B)]
               [(bot-set? B)    #f]
               [(top-set? B)    #t]
               [(bot-union? B)  (bot-bot-subseteq? A B)]
               [(top-union? B)  (bot-top-subseteq? A B)])]
        [(top-rect? A)
         (cond [(rect? B)       #f]
               [(top-rect? B)   (top-top-rect-subseteq? A B)]
               [(bot-set? B)    #f]
               [(top-set? B)    #f]
               [(bot-union? B)  #f]
               [(top-union? B)  (top-top-subseteq? A B)])]
        [(bot-set? A)
         (cond [(rect? B)       #f]
               [(top-rect? B)   #t]
               [(bot-set? B)    (bot-bot-set-subseteq? A B)]
               [(top-set? B)    (bot-top-set-subseteq? A B)]
               [(bot-union? B)  (bot-bot-subseteq? A B)]
               [(top-union? B)  (bot-top-subseteq? A B)])]
        [(top-set? A)
         (cond [(rect? B)       #f]
               [(top-rect? B)   #f]
               [(bot-set? B)    #f]
               [(top-set? B)    (top-top-set-subseteq? A B)]
               [(bot-union? B)  #f]
               [(top-union? B)  (top-top-subseteq? A B)])]
        [(bot-union? A)
         (cond [(or (bot-entry? B) (bot-union? B))  (bot-bot-subseteq? A B)]
               [(or (top-entry? B) (top-union? B))  (bot-top-subseteq? A B)])]
        [(top-union? A)
         (cond [(or (bot-entry? B) (bot-union? B))  #f]
               [(or (top-entry? B) (top-union? B))  (top-top-subseteq? A B)])]))

(: bot-bot-subseteq? ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union) -> Boolean))
(define (bot-bot-subseteq? A B)
  (andmap (λ: ([A : Bot-Entry])
            (set-subseteq? A (bot-union-ref B (bot-tag A))))
          (bot-union-sets A)))

(: bot-top-subseteq? ((U Bot-Entry Bot-Union) (U Top-Entry Top-Union) -> Boolean))
(define (bot-top-subseteq? A B)
  (andmap (λ: ([A : Bot-Entry])
            (set-subseteq? A (top-union-ref B (bot-tag A))))
          (bot-union-sets A)))

(: top-top-subseteq? ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> Boolean))
(define (top-top-subseteq? A B)
  (andmap (λ: ([B : Top-Entry])
            (set-subseteq? (top-union-ref A (get-tag B)) B))
          (top-union-sets B)))

(: pair-rect-subseteq? (Pair-Rect Pair-Rect -> Boolean))
(define (pair-rect-subseteq? A B)
  (and (set-subseteq? (pair-rect-fst A) (pair-rect-fst B))
       (set-subseteq? (pair-rect-snd A) (pair-rect-snd B))))

(: rect-subseteq? (Maybe-Rect Maybe-Rect -> Boolean))
(define (rect-subseteq? A B)
  (cond [(empty-set? A)  #t]
        [(empty-set? B)  #f]
        [(and (interval? A) (interval? B))  (interval-subseteq? A B)]
        [(and (null-rect? A) (null-rect? B))  #t]
        [(and (pair-rect? A) (pair-rect? B))  (pair-rect-subseteq? A B)]
        [(and (boolean-rect? A) (boolean-rect? B))  (boolean-rect-subseteq? A B)]
        [else  #f]))

(: bot-bot-rect-subseteq? (Rect Rect -> Boolean))
(define (bot-bot-rect-subseteq? A B)
  (rect-subseteq? A B))

(: bot-top-rect-subseteq? (Rect Top-Rect -> Boolean))
(define (bot-top-rect-subseteq? A B)
  (match-define (Tagged b-tag Bsub) B)
  (or (not (eq? (rect-tag A) b-tag))
      (rect-subseteq? A Bsub)))

(: top-top-rect-subseteq? (Top-Rect Top-Rect -> Boolean))
(define (top-top-rect-subseteq? A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (and (eq? a-tag b-tag)
       (rect-subseteq? Asub Bsub)))

(: bot-bot-set-subseteq? (Bot-Set Bot-Set -> Boolean))
(define (bot-bot-set-subseteq? A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (and (eq? a-tag b-tag)
       (set-subseteq? Asub Bsub)))

(: bot-top-set-subseteq? (Bot-Set Top-Set -> Boolean))
(define (bot-top-set-subseteq? A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (or (not (eq? a-tag b-tag))
      (set-subseteq? Asub Bsub)))

(: top-top-set-subseteq? (Top-Set Top-Set -> Boolean))
(define (top-top-set-subseteq? A B)
  (match-define (Tagged a-tag Asub) A)
  (match-define (Tagged b-tag Bsub) B)
  (and (eq? a-tag b-tag)
       (set-subseteq? Asub Bsub)))
