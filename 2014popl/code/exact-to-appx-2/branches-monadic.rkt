#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

(struct: branches ([values : (Listof Boolean)]) #:transparent)

(: branches-first (branches -> (U Boolean Bottom)))
(define (branches-first bs)
  (let ([bs  (branches-values bs)])
    (if (empty? bs) bottom (first bs))))

(: branches-rest (branches -> (U branches Bottom)))
(define (branches-rest bs)
  (let ([bs  (branches-values bs)])
    (if (empty? bs) bottom (branches (rest bs)))))

(define some-branches
  (list->set
   (for*/list: : (Listof branches) ([b0  (in-list '(#t #f))]
                                    [b1  (in-list '(#t #f))]
                                    [b2  (in-list '(#t #f))])
     (branches (list b0 b1 b2)))))
