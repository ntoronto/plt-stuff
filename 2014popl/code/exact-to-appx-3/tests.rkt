#lang typed/racket

(require typed/rackunit
         "bot-arrow.rkt"
         "map-arrow.rkt"
         "pre-arrow.rkt"
         "partial-arrows.rkt"
         "mapping.rkt"
         "preimage.rkt"
         "branches.rkt"
         "types.rkt"
         "../set-ops.rkt"
         )

(: ap/pbot (All (X Y) ((PBot-Arrow X Y) X -> Y)))
(define (ap/pbot f x)
  (define ys (set-image (f '()) (set-product some-branches (set x))))
  (set-take ((inst set-filter-out Y Bottom) bottom? ys)))


(: test-map-arrow (All (X Y) ((Bot-Arrow X Y) (Map-Arrow X Y) (Setof X) -> Any)))
(define (test-map-arrow comp/bot comp/map A)
  (define comp/map* ((inst lift/map X Y) comp/bot))
  (check-true (equal? (comp/map* A) (comp/map A))))

(: test-pre-arrow (All (X Y) ((Map-Arrow X Y) (Pre-Arrow X Y) (Setof X) -> Any)))
(define (test-pre-arrow comp/map comp/pre A)
  (define comp/pre* ((inst lift/pre X Y) comp/map))
  (parameterize ([pmapping-approx?  #f])
    (check-true (pmapping-equal? (comp/pre* A) (comp/pre A))))
  (parameterize ([pmapping-approx?  #t])
    (check-true (pmapping-subset? (comp/pre* A) (comp/pre A)))))

(: test-pbot-arrow (All (X Y) ((Bot-Arrow X Y) (PBot-Arrow X Y) (Setof X) -> Any)))
(define (test-pbot-arrow comp/bot comp/pbot A)
  (for: ([x  (in-list (set->list A))])
    (check-true (equal? (ap/pbot comp/pbot x) (comp/bot x)))))

(: test-pmap-arrow (All (X Y) ((PBot-Arrow X Y) (PMap-Arrow X Y) (Setof X) -> Any)))
(define (test-pmap-arrow comp/pbot comp/pmap A)
  (define comp/pmap* ((inst lift/pmap X Y) comp/pbot))
  (define A* (set-product some-branches A))
  (check-true (equal? ((comp/pmap* '()) A*) ((comp/pmap '()) A*))))

(: test-ppre-arrow (All (X Y) ((PMap-Arrow X Y) (PPre-Arrow X Y) (Setof X) -> Any)))
(define (test-ppre-arrow comp/pmap comp/ppre A)
  (define comp/ppre* ((inst lift/ppre X Y) comp/pmap))
  (define A* (set-product some-branches A))
  (parameterize ([pmapping-approx?  #f])
    (check-true (pmapping-equal? ((comp/ppre* '()) A*) ((comp/ppre '()) A*))))
  (parameterize ([pmapping-approx?  #t])
    (check-true (pmapping-subset? ((comp/ppre* '()) A*) ((comp/ppre '()) A*)))))

(printf "Testing add1...~n")
(define add1/bot ((inst arr/bot Integer Integer) add1))
(define add1/map ((inst arr/map Integer Integer) add1))
(define add1/pre ((inst arr/pre Integer Integer) add1))
(define add1/pbot ((inst arr/pbot Integer Integer) add1))
(define add1/pmap ((inst arr/pmap Integer Integer) add1))
(define add1/ppre ((inst arr/ppre Integer Integer) add1))
(test-map-arrow add1/bot add1/map (set 1 2 3))
(test-pre-arrow add1/map add1/pre (set 1 2 3))
(test-pbot-arrow add1/bot add1/pbot (set 1 2 3))
(test-pmap-arrow add1/pbot add1/pmap (set 1 2 3))
(test-ppre-arrow add1/pmap add1/ppre (set 1 2 3))

(printf "Testing sub1...~n")
(define sub1/bot ((inst arr/bot Integer Integer) sub1))
(define sub1/map ((inst arr/map Integer Integer) sub1))
(define sub1/pre ((inst arr/pre Integer Integer) sub1))
(define sub1/pbot ((inst arr/pbot Integer Integer) sub1))
(define sub1/pmap ((inst arr/pmap Integer Integer) sub1))
(define sub1/ppre ((inst arr/ppre Integer Integer) sub1))
(test-map-arrow sub1/bot sub1/map (set 1 2 3))
(test-pre-arrow sub1/map sub1/pre (set 1 2 3))
(test-pbot-arrow sub1/bot sub1/pbot (set 1 2 3))
(test-pmap-arrow sub1/pbot sub1/pmap (set 1 2 3))
(test-ppre-arrow sub1/pmap sub1/ppre (set 1 2 3))

(printf "Testing int-id...~n")
(define int-id/bot (>>>/bot add1/bot sub1/bot))
(define int-id/map (>>>/map add1/map sub1/map))
(define int-id/pre (>>>/pre add1/pre sub1/pre))
(define int-id/pbot (>>>/pbot add1/pbot sub1/pbot))
(define int-id/pmap (>>>/pmap add1/pmap sub1/pmap))
(define int-id/ppre (>>>/ppre add1/ppre sub1/ppre))
(test-map-arrow int-id/bot int-id/map (set 1 2 3))
(test-pre-arrow int-id/map int-id/pre (set 1 2 3))
(test-pbot-arrow int-id/bot int-id/pbot (set 1 2 3))
(test-pmap-arrow int-id/pbot int-id/pmap (set 1 2 3))
(test-ppre-arrow int-id/pmap int-id/ppre (set 1 2 3))

(printf "Testing 1...~n")
(define 1/bot ((inst const/bot Symbol Integer) 1))
(define 1/map ((inst const/map Symbol Integer) 1))
(define 1/pre ((inst const/pre Symbol Integer) 1))
(define 1/pbot ((inst const/pbot Symbol Integer) 1))
(define 1/pmap ((inst const/pmap Symbol Integer) 1))
(define 1/ppre ((inst const/ppre Symbol Integer) 1))
(test-map-arrow 1/bot 1/map (set 'a 'b 'c))
(test-pre-arrow 1/map 1/pre (set 'a 'b 'c))
(test-pbot-arrow 1/bot 1/pbot (set 'a 'b 'c))
(test-pmap-arrow 1/pbot 1/pmap (set 'a 'b 'c))
(test-ppre-arrow 1/pmap 1/ppre (set 'a 'b 'c))

(printf "Testing pair12...~n")
(define pair12/bot (pair/bot 1/bot ((inst const/bot Symbol Integer) 2)))
(define pair12/map (pair/map 1/map ((inst const/map Symbol Integer) 2)))
(define pair12/pre (pair/pre 1/pre ((inst const/pre Symbol Integer) 2)))
(define pair12/pbot (pair/pbot 1/pbot ((inst const/pbot Symbol Integer) 2)))
(define pair12/pmap (pair/pmap 1/pmap ((inst const/pmap Symbol Integer) 2)))
(define pair12/ppre (pair/ppre 1/ppre ((inst const/ppre Symbol Integer) 2)))
(test-map-arrow pair12/bot pair12/map (set 'a 'b 'c))
(test-pre-arrow pair12/map pair12/pre (set 'a 'b 'c))
(test-pbot-arrow pair12/bot pair12/pbot (set 'a 'b 'c))
(test-pmap-arrow pair12/pbot pair12/pmap (set 'a 'b 'c))
(test-ppre-arrow pair12/pmap pair12/ppre (set 'a 'b 'c))

(printf "Testing int-dup...~n")
(define int-dup/bot (pair/bot int-id/bot int-id/bot))
(define int-dup/map (pair/map int-id/map int-id/map))
(define int-dup/pre (pair/pre int-id/pre int-id/pre))
(define int-dup/pbot (pair/pbot int-id/pbot int-id/pbot))
(define int-dup/pmap (pair/pmap int-id/pmap int-id/pmap))
(define int-dup/ppre (pair/ppre int-id/ppre int-id/ppre))
(test-map-arrow int-dup/bot int-dup/map (set 1 2))
(test-pre-arrow int-dup/map int-dup/pre (set 1 2))
(test-pbot-arrow int-dup/bot int-dup/pbot (set 1 2))
(test-pmap-arrow int-dup/pbot int-dup/pmap (set 1 2))
(test-ppre-arrow int-dup/pmap int-dup/ppre (set 1 2))

(printf "Testing int-pair-id...~n")
(define int-pair-id/bot (pair/bot (inst fst/bot Integer Integer) (inst snd/bot Integer Integer)))
(define int-pair-id/map (pair/map (inst fst/map Integer Integer) (inst snd/map Integer Integer)))
(define int-pair-id/pre (pair/pre (inst fst/pre Integer Integer) (inst snd/pre Integer Integer)))
(define int-pair-id/pbot (pair/pbot (inst fst/pbot Integer Integer) (inst snd/pbot Integer Integer)))
(define int-pair-id/pmap (pair/pmap (inst fst/pmap Integer Integer) (inst snd/pmap Integer Integer)))
(define int-pair-id/ppre (pair/ppre (inst fst/ppre Integer Integer) (inst snd/ppre Integer Integer)))
(test-map-arrow int-pair-id/bot int-pair-id/map (set '(1 . 4) '(2 . 5)))
(test-pre-arrow int-pair-id/map int-pair-id/pre (set '(1 . 4) '(2 . 5)))
(test-pbot-arrow int-pair-id/bot int-pair-id/pbot (set '(1 . 4) '(2 . 5)))
(test-pmap-arrow int-pair-id/pbot int-pair-id/pmap (set '(1 . 4) '(2 . 5)))
(test-ppre-arrow int-pair-id/pmap int-pair-id/ppre (set '(1 . 4) '(2 . 5)))

(printf "Testing halt-on-true...~n")

(: halt-on-true/bot (Bot-Arrow Boolean Boolean))
(define halt-on-true/bot
  (if/bot (inst id/bot Boolean)
          (λ () ((inst const/bot Boolean Boolean) #t))
          (λ () halt-on-true/bot)))

(: halt-on-true/map (Map-Arrow Boolean Boolean))
(define halt-on-true/map
  (if/map (inst id/map Boolean)
          (λ () ((inst const/map Boolean Boolean) #t))
          (λ () halt-on-true/map)))

(: halt-on-true/pre (Pre-Arrow Boolean Boolean))
(define halt-on-true/pre
  (if/pre (inst id/pre Boolean)
          (λ () ((inst const/pre Boolean Boolean) #t))
          (λ () halt-on-true/pre)))

(: halt-on-true/pbot (PBot-Arrow Boolean Boolean))
(define halt-on-true/pbot
  (if/pbot (inst id/pbot Boolean)
           (λ () ((inst const/pbot Boolean Boolean) #t))
           (λ () halt-on-true/pbot)))

(: halt-on-true/pmap (PMap-Arrow Boolean Boolean))
(define halt-on-true/pmap
  (if/pmap (inst id/pmap Boolean)
           (λ () ((inst const/pmap Boolean Boolean) #t))
           (λ () halt-on-true/pmap)))

(: halt-on-true/ppre (PPre-Arrow Boolean Boolean))
(define halt-on-true/ppre
  (if/ppre (inst id/ppre Boolean)
           (λ () ((inst const/ppre Boolean Boolean) #t))
           (λ () halt-on-true/ppre)))

(test-map-arrow halt-on-true/bot halt-on-true/map (set #t))
(test-pre-arrow halt-on-true/map halt-on-true/pre (set #t))
(test-pbot-arrow halt-on-true/bot halt-on-true/pbot (set #t))
(test-pmap-arrow halt-on-true/pbot halt-on-true/pmap (set #t #f))
(test-ppre-arrow halt-on-true/pmap halt-on-true/ppre (set #t #f))
