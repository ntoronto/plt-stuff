#lang typed/racket/base

(require racket/flonum
         racket/match
         racket/list
         "omega.rkt"
         "rect.rkt")

(provide (all-defined-out))

(define-type Interval-Splitter
  (Interval Flonum -> (Values (Listof Interval) (Listof Positive-Flonum))))

(define-type Indexes (Listof (U Omega-Idx
                                interval-index
                                if-indexes
                                (Pair interval-index Natural))))

(struct: interval-index ([index : Omega-Idx] [split : Interval-Splitter])
  #:transparent)
(struct: if-indexes ([index : Omega-Idx] [true : (Promise Indexes)] [false : (Promise Indexes)])
  #:transparent)

(: interval-split Interval-Splitter)
(define (interval-split I min-ivl)
  (match-define (interval a b a? b?) I)
  (cond [((fl- b a) . fl<= . min-ivl)  (values (list I) (list 1.0))]
        [else
         (define c (* 0.5 (+ a b)))
         (define m1 (fl- c a))
         (define m2 (fl- b c))
         (cond [(and (positive? m1) (positive? m2))
                (values (list (Interval a c a? #t) (Interval c b #f b?)) (list m1 m2))]
               [else
                (values (list I) (list 1.0))])]))

(: intersect-and-filter ((Listof Interval) Interval -> (Values (Listof Interval)
                                                               (Listof Positive-Flonum))))
(define (intersect-and-filter Is A)
  (let: loop ([Is Is] [new-Is : (Listof Interval)  empty] [ps : (Listof Positive-Flonum)  empty])
    (cond [(empty? Is)  (values (reverse new-Is) (reverse ps))]
          [else
           (define I (interval-intersect (first Is) A))
           (cond [(empty-set? I)  (loop (rest Is) new-Is ps)]
                 [else
                  (define p (interval-measure I))
                  (cond [(p . <= . 0.0)  (loop (rest Is) new-Is ps)]
                        [else  (loop (rest Is) (cons I new-Is) (cons p ps))])])])))

(: make-constant-splitter ((Listof Interval) -> Interval-Splitter))
(define (make-constant-splitter Is)
  (when (not (apply rect-disjoint? Is))
    (raise-argument-error 'make-binary-split "disjoint (Listof Interval)" Is))
  (let-values ([(Is _)  (intersect-and-filter Is unit-interval)])
    (λ (A _) (intersect-and-filter Is A))))
