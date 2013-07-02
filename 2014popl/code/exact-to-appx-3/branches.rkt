#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

(define-type Leaf 'leaf)
(define leaf 'leaf)
(define-predicate leaf? Leaf)

(struct: branches-node ([value : Boolean] [fst : Branches] [snd : Branches]) #:transparent)
(define-type Branches (U branches-node 'leaf))

(define-type Tree-Index (Listof (U 0 1)))

(: branches-ref (Branches Tree-Index -> (U Boolean Bottom)))
(define (branches-ref b i)
  (cond [(leaf? b)  bottom]
        [(empty? i)  (branches-node-value b)]
        [(zero? (first i))  (branches-ref (branches-node-fst b) (rest i))]
        [else               (branches-ref (branches-node-snd b) (rest i))]))

(: moar-branches (Branches -> (Listof Branches)))
(define (moar-branches b)
  (cond [(leaf? b)
         (list (branches-node #t leaf leaf) (branches-node #f leaf leaf))]
        [else
         (match-define (branches-node v fst snd) b)
         (for*/list: : (Listof Branches) ([b0  (in-list (moar-branches fst))]
                                          [b1  (in-list (moar-branches snd))])
           (branches-node v b0 b1))]))

(: add-branches ((Listof Branches) -> (Listof Branches)))
(define (add-branches bs)
  (remove-duplicates (append* (cons bs (map moar-branches bs)))))

(define some-branches (list->set (add-branches (add-branches (list leaf)))))
