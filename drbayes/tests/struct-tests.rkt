#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (except-in "../main.rkt"))

(struct/drbayes point (x y))

(define f-expr
  (drbayes (let ([xy  (point (uniform) (uniform))])
             (lazy-if ((point-x xy) . < . (point-y xy))
                      (fail)
                      xy))))

(let-values ([(ps ws)  (drbayes-sample f-expr 20)])
  (map point-x ps))
