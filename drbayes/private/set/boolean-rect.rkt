#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base)
         "extremal-set.rkt"
         "../untyped-utils.rkt")

(: print-boolean-rect (Boolean-Rect Output-Port (U #t #f 0 1) -> Any))
(define (print-boolean-rect A port mode)
  (fprintf port "~a" (Boolean-Rect-name A)))

(struct: Boolean-Rect ([name : Symbol])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-boolean-rect)

(define trues (Boolean-Rect 'trues))
(define falses (Boolean-Rect 'falses))
(define booleans (Boolean-Rect 'booleans))

(define-syntax boolean-rect? (make-rename-transformer #'Boolean-Rect?))

(define-type Maybe-Boolean-Rect (U Empty-Set Boolean-Rect))

(: boolean-rect-member? (Boolean-Rect Boolean -> Boolean))
(define (boolean-rect-member? A x)
  (or (eq? A booleans)
      (and (eq? A trues) (eq? x #t))
      (and (eq? A falses) (eq? x #f))))

(: boolean-rect-join (Boolean-Rect Boolean-Rect -> Boolean-Rect))
(define (boolean-rect-join A B)
  (cond [(eq? A booleans)  A]
        [(eq? B booleans)  B]
        [(eq? A B)  A]
        [else  booleans]))

(: boolean-rect-intersect (Boolean-Rect Boolean-Rect -> Maybe-Boolean-Rect))
(define (boolean-rect-intersect A B)
  (cond [(eq? A booleans)  B]
        [(eq? B booleans)  A]
        [(eq? A B)  A]
        [else  empty-set]))

(: boolean-rect-subseteq? (Boolean-Rect Boolean-Rect -> Boolean))
(define (boolean-rect-subseteq? A B)
  (cond [(eq? B booleans)  #t]
        [(eq? A B)  #t]
        [else  #f]))

(: booleans->boolean-rect (Boolean Boolean -> Maybe-Boolean-Rect))
(define (booleans->boolean-rect t? f?)
  (cond [t?    (if f? booleans trues)]
        [else  (if f? falses empty-set)]))

(: boolean-rect->booleans (Maybe-Boolean-Rect -> (Values Boolean Boolean)))
(define (boolean-rect->booleans A)
  (cond [(eq? A booleans)  (values #t #t)]
        [(eq? A trues)     (values #t #f)]
        [(eq? A falses)    (values #f #t)]
        [else              (values #f #f)]))
