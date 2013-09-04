#lang typed/racket/base

(require racket/flonum
         racket/match
         racket/list
         "../set/tree-value.rkt"
         "../set/extremal-set.rkt"
         "../set/real-set.rkt")

(provide (all-defined-out))

(: interval-max-splits (Parameterof Natural))
(define interval-max-splits (make-parameter 5))

(: interval-min-length (Parameterof Nonnegative-Flonum))
(define interval-min-length (make-parameter 1e-14))

(define-type Interval-Splitter
  (Nonextremal-Interval -> (Values (Listof Nonextremal-Interval) (Listof Positive-Flonum))))

(define-type Indexes (Listof (U interval-index if-indexes)))
(struct: interval-index ([index : Tree-Index] [split : (U #f Interval-Splitter)]) #:transparent)
(struct: if-indexes ([index : Tree-Index] [true : (-> Indexes)] [false : (-> Indexes)])
  #:transparent)

(: intersect-and-filter ((Listof Nonextremal-Interval) Nonextremal-Interval
                                                       -> (Values (Listof Nonextremal-Interval)
                                                                  (Listof Positive-Flonum))))
(define (intersect-and-filter Is A)
  (let: loop ([Is Is] [new-Is : (Listof Nonextremal-Interval)  empty]
                      [ps : (Listof Positive-Flonum)  empty])
    (cond [(empty? Is)  (values (reverse new-Is) (reverse ps))]
          [else
           (define I (interval-intersect (first Is) A))
           (cond [(empty-real-set? I)  (loop (rest Is) new-Is ps)]
                 [else
                  (define p (interval-measure I))
                  (cond [(p . <= . 0.0)  (loop (rest Is) new-Is ps)]
                        [else  (loop (rest Is) (cons I new-Is) (cons p ps))])])])))

(: make-constant-splitter ((Listof Nonextremal-Interval) -> Interval-Splitter))
(define (make-constant-splitter Is)
  (let-values ([(Is _)  (intersect-and-filter Is unit-interval)])
    (λ (A) (intersect-and-filter Is A))))
