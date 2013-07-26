#lang typed/racket/base

(require (prefix-in : "../set-ops.rkt")
         (prefix-in : racket/set)
         racket/match)

(provide (all-defined-out))

(define +inf '+∞)
(define-type +Inf '+∞)
(define-predicate +inf? +Inf)

(define -inf '-∞)
(define-type -Inf '-∞)
(define-predicate -inf? -Inf)

;(struct: (X) finite ([value : X]) #:transparent)

(define-type (Extended X) (U +Inf -Inf X))

(: generic<= (All (X) ((Extended X) (Extended X) -> Boolean)))
(define (generic<= a1 a2)
  (cond [(+inf? a2)  #t]
        [(-inf? a1)  #t]
        [(-inf? a2)  #f]
        [(+inf? a1)  #f]
        [(and (real? a1) (real? a2))  (<= a1 a2)]
        [(and (pair? a1) (pair? a2))  (and (generic<= (car a1) (car a2))
                                           (generic<= (cdr a1) (cdr a2)))]
        [else  (error 'generic<= "expected comparable values; given ~e and ~e" a1 a2)]))

(: pair-min (All (X1 X2) ((Pair (Extended X1) (Extended X2))
                          (Pair (Extended X1) (Extended X2))
                          -> (Pair (Extended X1) (Extended X2)))))
(define (pair-min a1 a2)
  (cons (generic-min (cons (car a1) (car a2)))
        (generic-min (cons (cdr a1) (cdr a2)))))

(define-predicate real-pair? (Pair Real Real))
(define-predicate pair-pair? (Pair (Pair Any Any) (Pair Any Any)))

(: generic-min (All (X) ((Pair (U X -Inf +Inf) (U X -Inf +Inf)) -> (U X -Inf +Inf))))
(define (generic-min a)
  (match-define (cons a1 a2) a)
  (cond [(or (-inf? a1) (-inf? a2))  -inf]
        [(+inf? a1)  a2]
        [(+inf? a2)  a1]
        [(real-pair? a)  (if (generic<= a1 a2) a1 a2)]
        [(and (pair? a1) (pair? a2))  (pair-min a1 a2)]
        [else  (error 'generic-min "expected comparable values; given ~e and ~e" a1 a2)]))

#|
(struct: (X1 X2) box ([min : X1] [max : X2]) #:transparent)


(: box-intersect (All (X1 X2) ((box X1 X2) (box X1 X2) -> (box X1 X2))))
(define (box-intersect A1 A2)
  (match-define (box A1-min A1-max) A1)
  (match-define (box A2-min A2-max) A2)
  (error 'unimplemented))
|#
