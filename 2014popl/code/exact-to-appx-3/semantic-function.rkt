#lang typed/racket

(require typed/rackunit
         "bot-arrow.rkt")

(define-syntax (env stx) (raise-syntax-error 'env "cannot be used as an expression" stx))

(: plus ((Pair Number Number) -> Number))
(define (plus ab)
  (+ (car ab) (cdr ab)))

(: minus ((Pair Number Number) -> Number))
(define (minus ab)
  (- (car ab) (cdr ab)))

(: neg (Number -> Number))
(define (neg x) (- x))

(: let/bot (All (X Y) ((Bot-Arrow X Y) -> (All (Z) (Bot-Arrow (Pair Y X) Z) -> (Bot-Arrow X Z)))))
(define ((let/bot expr) body)
  (>>>/bot (pair/bot expr (inst id/bot X)) body))

(define-for-syntax (S stx)
  ;(printf "stx = ~v~n" (syntax->datum stx))
  (syntax-case stx (cons car cdr if let env Pair + - neg)
    [(cons e1 e2)
     #`(pair/bot #,(S #'e1) #,(S #'e2))]
    [(car (Pair X Y) e)
     #`(>>>/bot #,(S #'e) (inst fst/bot X Y))]
    [(cdr (Pair X Y) e)
     #`(>>>/bot #,(S #'e) (inst snd/bot X Y))]
    [(if ec et ef)
     #`(if/bot #,(S #'ec) (lazy/bot (λ () #,(S #'et))) (lazy/bot (λ () #,(S #'ef))))]
    [(let E ex X eb)
     #`(((inst let/bot E X) #,(S #'ex)) #,(S #'eb))]
    [(env (Pair E0 E) 0)
     #`(inst fst/bot E0 E)]
    [(env (Pair E0 E) n)
     (exact-nonnegative-integer? (syntax->datum #'n))
     (let ([n  (syntax->datum #'n)])
       #`(>>>/bot (inst snd/bot E0 E) #,(S #`(env E #,(- n 1)))))]
    [(+ e1 e2)
     #`(>>>/bot (pair/bot #,(S #'e1) #,(S #'e2)) (arr/bot plus))]
    [(- e1 e2)
     #`(>>>/bot (pair/bot #,(S #'e1) #,(S #'e2)) (arr/bot minus))]
    [(neg e)
     #`(>>>/bot #,(S #'e) (arr/bot neg))]
    [(ef ex)
     #`(>>>/bot #,(S #'(cons ex null)) ef)]
    [n
     #`(const/bot n)]
    ))

(define-syntax (meaningof stx)
  (syntax-case stx ()
    [(_ e)  (S #'e)]))

(check-equal? ((meaningof 4) null) 4)
(check-equal? ((meaningof (+ 4 5)) null) 9)
(check-equal? ((meaningof (cons 4 5)) null) (cons 4 5))
(check-equal? ((meaningof (if #t 4 5)) null) 4)
(check-equal? ((meaningof (if #f 4 5)) null) 5)

(check-equal? ((meaningof (let Null 4 Number
                            (env (Pair Number Null) 0)))
               null)
              4)

(check-equal? ((meaningof (let Null 4 Number
                            (let (Pair Number Null) 5 Number
                              (- (env (Pair Number (Pair Number Null)) 0)
                                 (env (Pair Number (Pair Number Null)) 1)))))
               null)
              1)

(define negate (meaningof (neg (env (Pair Number Null) 0))))
(check-equal? ((meaningof (negate 4)) null) -4)


(define sub (meaningof (- (car (Pair Number Number) (env (Pair (Pair Number Number) Null) 0))
                          (cdr (Pair Number Number) (env (Pair (Pair Number Number) Null) 0)))))
(check-equal? ((meaningof (sub (cons 4 5))) null) -1)
