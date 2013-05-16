#lang typed/racket

(require "../private/set/pair-set.rkt"
         "../private/set/real-set.rkt"
         "../private/utils.rkt"
         "rackunit-utils.rkt"
         "random-real-set.rkt"
         "profile.rkt")

(printf "starting...~n")

;; Using `inst' makes type checking very fast; but without it, inferring the type parameters takes a
;; long, long time (may be exponential or looping forever)

(define real-set-sig
  ((inst set-sig Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   real-set-member?
   reals?
   empty-real-set?
   reals
   empty-real-set
   real-set-complement
   real-set-subtract
   real-set-intersect
   real-set-union
   ))

(define-type Nonextremal-Real-Pair-Set (Pair-Set Nonextremal-Real-Set Full-Real-Set))
(define-type Nonfull-Real-Pair-Set (U Nonextremal-Real-Pair-Set Empty-Pair-Set))
(define-type Nonempty-Real-Pair-Set (U Nonextremal-Real-Pair-Set Full-Pair-Set))
(define-type Real-Pair-Set (U Nonextremal-Real-Pair-Set Full-Pair-Set Empty-Pair-Set))

(define pair-set
  (make-pair-set Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum real-set-sig))

(define pair-set-complement
  ((inst make-pair-set-complement Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   real-set-sig))

(define pair-set-subtract
  ((inst make-pair-set-subtract Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   real-set-sig))

(define pair-set-intersect
  ((inst make-pair-set-intersect Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   real-set-sig))

(define pair-set-union
  ((inst make-pair-set-union Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   real-set-sig))

(define pair-set-subseteq?
  ((inst make-pair-set-subseteq? Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   real-set-sig))

(define pair-set-member?
  ((inst make-pair-set-member? Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   real-set-sig))

(: pair-set-equal? (Real-Pair-Set Real-Pair-Set -> Boolean))
(define (pair-set-equal? A B)
  (and (pair-set-subseteq? A B) (pair-set-subseteq? B A)))

(: pair-rect-measure ((Nonextremal-Pair-Rect Nonextremal-Real-Set Full-Real-Set) -> Flonum))
(define (pair-rect-measure A)
  (match-define (Nonextremal-Pair-Rect A1 A2) A)
  (* (real-set-measure A1) (real-set-measure A2)))

(: random-pair-set (-> Real-Pair-Set))
(define (random-pair-set)
  (define r (random))
  (cond [(r . < . 0.1)  pairs]
        [(r . < . 0.2)  empty-pair-set]
        [else
         (let loop ()
           (define A (pair-set (random-real-set)
                               (random-real-set)))
           (if (or (pairs? A) (empty-pair-set? A)) (loop) A))]))

(: random-pair/rect ((Nonextremal-Pair-Rect Nonextremal-Real-Set Full-Real-Set)
                     -> (Pair Flonum Flonum)))
(define (random-pair/rect A)
  (match-define (Nonextremal-Pair-Rect A1 A2) A)
  (cons (random-real A1) (random-real A2)))

(: random-pair (Real-Pair-Set -> (Pair Flonum Flonum)))
(define (random-pair A)
  (cond [(empty-pair-set? A)  (cons +nan.0 +nan.0)]
        [(pairs? A)  (cons (random-real reals)
                           (random-real reals))]
        [(Nonextremal-Pair-Rect? A)
         (random-pair/rect A)]
        [else
         (define As (Nonextremal-Pair-Rect-List-elements A))
         (define i (sample-index (normalize-probs/+2 (map/+2 pair-rect-measure As))))
         (random-pair/rect (list-ref As i))]))


(time
 (for: ([_  (in-range 100000)])
   (check-set-algebra pair-set-equal?
                      pair-set-member?
                      pair-set-subseteq?
                      empty-pair-set
                      pairs
                      pair-set-subtract
                      pair-set-union
                      pair-set-intersect
                      random-pair-set
                      random-pair)
   (check-bounded-lattice pair-set-equal?
                          pair-set-subseteq?
                          pair-set-union
                          pair-set-intersect
                          empty-pair-set
                          pairs
                          random-pair-set)))
