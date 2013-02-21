#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         "../main.rkt")

(define/drbayes (S)
  (lazy-if (boolean (const 0.5)) (T) (F)))

(define/drbayes (T)
  (lazy-cond [(boolean (const 0.4))  (cons #t (T))]
             [(boolean (const 0.5))  (cons #t (F))]
             [else  null]))

(define/drbayes (F)
  (lazy-cond [(boolean (const 0.4))  (cons #f (F))]
             [(boolean (const 0.5))  #;(cons #f (T))
                                     (cons #f (let ([s  (T)])
                                                (strict-if (list-ref s (const 1))
                                                           (fail)
                                                           s)))]
             [else  null]))

#;
(drbayes-sample (drbayes (S)) 20)

(drbayes-sample (drbayes (S))
                20
                (pair-rect 'tf (pair-rect 't universal-set)))

(print-sampler-stats)
