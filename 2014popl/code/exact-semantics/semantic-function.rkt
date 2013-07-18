#lang typed/racket

(require typed/rackunit
         "../types.rkt"
         "../set-ops.rkt"
         "../branch-trace.rkt"
         "bot-arrow.rkt"
         "map-arrow.rkt"
         "pre-arrow.rkt"
         "partial-arrows.rkt"
         )

(define-type (Arrow X Y) (PBot-Arrow X Y))
(define arr arr/pbot)
(define >>> >>>/pbot)
(define pair pair/pbot)
(define lazy lazy/pbot)
(define ifte if/pbot)
(define id id/pbot)
(define fst fst/pbot)
(define snd snd/pbot)
(define const const/pbot)

(: run (All (X Y) ((PBot-Arrow Null Y) -> (U Bottom Y))))
(define (run f)
  (unjust (ap/pbot f null)))

#|
(define-type (Arrow X Y) (Bot-Arrow X Y))
(define arr arr/bot)
(define >>> >>>/bot)
(define pair pair/bot)
(define lazy lazy/bot)
(define ifte if/bot)
(define id id/bot)
(define fst fst/bot)
(define snd snd/bot)
(define const const/bot)

(: run (All (X Y) ((Bot-Arrow Null Y) -> (U Bottom Y))))
(define (run f)
  (unjust (f null)))
|#


(define-syntax (env stx) (raise-syntax-error 'env "cannot be used as an expression" stx))

(: plus ((Pair Number Number) -> Number))
(define (plus ab)
  (+ (car ab) (cdr ab)))

(: minus ((Pair Number Number) -> Number))
(define (minus ab)
  (- (car ab) (cdr ab)))

(: neg (Number -> Number))
(define (neg x) (- x))

(: lett (All (X Y) ((Arrow X Y) -> (All (Z) (Arrow (Pair Y X) Z) -> (Arrow X Z)))))
(define ((lett expr) body)
  (>>> (pair expr (inst id X)) body))

(define-for-syntax (S stx)
  ;(printf "stx = ~v~n" (syntax->datum stx))
  (syntax-case stx (cons car cdr if let env Pair + - neg)
    [(cons e1 e2)
     #`(pair #,(S #'e1) #,(S #'e2))]
    [(car (Pair X Y) e)
     #`(>>> #,(S #'e) (inst fst X Y))]
    [(cdr (Pair X Y) e)
     #`(>>> #,(S #'e) (inst snd X Y))]
    [(if ec et ef)
     #`(ifte #,(S #'ec) (lazy (λ () #,(S #'et))) (lazy (λ () #,(S #'ef))))]
    [(let E ex X eb)
     #`(((inst lett E X) #,(S #'ex)) #,(S #'eb))]
    [(env (Pair E0 E) 0)
     #`(inst fst E0 E)]
    [(env (Pair E0 E) n)
     (exact-nonnegative-integer? (syntax->datum #'n))
     (let ([n  (syntax->datum #'n)])
       #`(>>> (inst snd E0 E) #,(S #`(env E #,(- n 1)))))]
    [(+ e1 e2)
     #`(>>> (pair #,(S #'e1) #,(S #'e2)) (arr plus))]
    [(- e1 e2)
     #`(>>> (pair #,(S #'e1) #,(S #'e2)) (arr minus))]
    [(neg e)
     #`(>>> #,(S #'e) (arr neg))]
    [(ef ex)
     #`(>>> #,(S #'(cons ex null)) ef)]
    [n
     #`(const n)]
    ))

(define-syntax (meaningof stx)
  (syntax-case stx ()
    [(_ e)  (S #'e)]))

(check-equal? (run (meaningof 4)) 4)
(check-equal? (run (meaningof (+ 4 5))) 9)
(check-equal? (run (meaningof (cons 4 5))) (cons 4 5))
(check-equal? (run (meaningof (if #t 4 5))) 4)
(check-equal? (run (meaningof (if #f 4 5))) 5)

(check-equal? (run (meaningof (let Null 4 Number
                                (env (Pair Number Null) 0))))
              4)

(check-equal? (run (meaningof (let Null 4 Number
                                (let (Pair Number Null) 5 Number
                                  (- (env (Pair Number (Pair Number Null)) 0)
                                     (env (Pair Number (Pair Number Null)) 1))))))
              1)

(define negate (meaningof (neg (env (Pair Number Null) 0))))
(check-equal? (run (meaningof (negate 4))) -4)


(define sub (meaningof (- (car (Pair Number Number) (env (Pair (Pair Number Number) Null) 0))
                          (cdr (Pair Number Number) (env (Pair (Pair Number Number) Null) 0)))))
(check-equal? (run (meaningof (sub (cons 4 5)))) -1)
