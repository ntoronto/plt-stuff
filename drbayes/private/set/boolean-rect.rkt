#lang typed/racket/base

(provide (all-defined-out))

(require "extremal-set.rkt")

(define-type Boolean-Rect (U 'tf 't 'f))
(define-type Maybe-Boolean-Rect (U Empty-Set Boolean-Rect))

(define (boolean-rect? x)
  (or (eq? x 'tf)
      (eq? x 't)
      (eq? x 'f)))

(: boolean-rect-member? (Boolean-Rect Boolean -> Boolean))
(define (boolean-rect-member? A x)
  (or (eq? A 'tf)
      (and (eq? A 't) (eq? x #t))
      (and (eq? A 'f) (eq? x #f))))

(: boolean-rect-join (Boolean-Rect Boolean-Rect -> Boolean-Rect))
(define (boolean-rect-join A B)
  (cond [(eq? A 'tf)  A]
        [(eq? B 'tf)  B]
        [(eq? A B)  A]
        [else  'tf]))

(: boolean-rect-intersect (Boolean-Rect Boolean-Rect -> Maybe-Boolean-Rect))
(define (boolean-rect-intersect A B)
  (cond [(eq? A 'tf)  B]
        [(eq? B 'tf)  A]
        [(eq? A B)  A]
        [else  empty-set]))

(: boolean-rect-subseteq? (Boolean-Rect Boolean-Rect -> Boolean))
(define (boolean-rect-subseteq? A B)
  (cond [(eq? B 'tf)  #t]
        [(eq? A B)  #t]
        [else  #f]))

(: booleans->boolean-rect (case-> (#t #t -> 'tf)
                                 (#t #f -> 't)
                                 (#f #t -> 'f)
                                 (#f #f -> Empty-Set)
                                 (Boolean Boolean -> Maybe-Boolean-Rect)))
(define (booleans->boolean-rect t? f?)
  (cond [t?    (if f? 'tf 't)]
        [else  (if f? 'f empty-set)]))

(: boolean-rect->booleans (case-> ('tf -> (Values #t #t))
                                 ('t -> (Values #t #f))
                                 ('f -> (Values #f #t))
                                 (Empty-Set -> (Values #f #f))
                                 (Maybe-Boolean-Rect -> (Values Boolean Boolean))))
(define (boolean-rect->booleans A)
  (case A
    [(tf)  (values #t #t)]
    [(t)   (values #t #f)]
    [(f)   (values #f #t)]
    [else  (values #f #f)]))
