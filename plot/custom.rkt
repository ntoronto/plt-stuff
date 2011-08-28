#lang racket/base

(require racket/contract
         "common/contract.rkt"
         "common/ticks.rkt")

(define tick-function/c (real? real? . -> . (listof tick?)))

(provide/contract
 (struct tick ([p real?] [label string?] [major? boolean?]))
 [tick-ps->majors  ((listof real?) positive-integer/c . -> . (listof boolean?))]
 [default-range->ticks  (positive-integer/c  . -> . tick-function/c)]
 [real->tick-label  (real? . -> . string?)]
 [any->tick-label  (any/c . -> . string?)]
 )

(provide plot-color/c
         tick-function/c)
