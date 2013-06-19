#lang typed/racket

(require "set-ops.rkt")

(provide standard-algebra)

(define-type Domain-Symbol (U 'a 'b 'c 'd 'bad))
(define-type Domain-Value (Rec Value (U Domain-Symbol Boolean (Pair Value Value))))
(define-type Domain-Pair (Pair Domain-Value Domain-Value))
(define-predicate domain-value? Domain-Value)
(define-predicate domain-symbol? Domain-Symbol)
(define-predicate domain-pair? Domain-Pair)
(define-predicate domain-set? (Setof Domain-Value))

(: list-pairs (All (X) ((Listof X) -> (Listof (Listof X)))))
(define (list-pairs xs)
  (cond [(empty? xs)  empty]
        [(empty? (rest xs))  (list (list (first xs)))]
        [else  (cons (list (first xs) (second xs))
                     (list-pairs (rest (rest xs))))]))

(define domain-symbol-algebra
  (set-algebra-close (set (set 'a 'b) (set 'c 'd) (set 'bad))))

(define domain-boolean-algebra
  (set-algebra-close (set (set #t) (set #f))))

(: symbol-algebra ((Setof Domain-Symbol) -> (Setof (Setof Domain-Symbol))))
(define (symbol-algebra A)
  (set-image (λ: ([B : (Setof Domain-Symbol)]) (set-intersect A B))
             domain-symbol-algebra))

(: boolean-algebra ((Setof Boolean) -> (Setof (Setof Boolean))))
(define (boolean-algebra A)
  (set-image (λ: ([B : (Setof Boolean)]) (set-intersect A B))
             domain-boolean-algebra))

(: pair-algebra ((Setof Domain-Pair) -> (Setof (Setof Domain-Pair))))
(define (pair-algebra A)
  (cond [(set-empty? A)  (set ((inst set Domain-Pair)))]
        [else
         (define A1s (domain-algebra (set-image (inst car Domain-Value Domain-Value) A)))
         (define A2s (domain-algebra (set-image (inst cdr Domain-Value Domain-Value) A)))
         (define As (set-product-algebra A1s A2s))
         (set-image (λ: ([B : (Setof Domain-Pair)]) (set-intersect A B)) As)]))

(: domain-algebra ((Setof Domain-Value) -> (Setof (Setof Domain-Value))))
(define (domain-algebra A)
  (define Asymb (list->set (filter domain-symbol? (set->list A))))
  (define Abool (list->set (filter boolean? (set->list A))))
  (define Apair (list->set (filter domain-pair? (set->list A))))
  (set-algebra-close (set-union (symbol-algebra Asymb)
                                (boolean-algebra Abool)
                                (pair-algebra Apair))))

(: standard-algebra (All (X) ((Setof X) -> (Setof (Setof X)))))
(define (standard-algebra orig-A)
  (define A orig-A)
  (cond [(domain-set? A)  (set-intersect (set-power orig-A) (domain-algebra A))]
        [else
         (define A1 (list->set (filter domain-value? (set->list A))))
         (define A2 (set-subtract A A1))
         (define A1s (domain-algebra A1))
         (define A2s (set-power A2))
         (set-intersect (set-power orig-A) (set-algebra-close (set-union A1s A2s)))]))
