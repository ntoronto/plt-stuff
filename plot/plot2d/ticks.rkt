#lang racket/base

(require racket/contract
         "../common/ticks.rkt"
         "../common/contract.rkt")

(provide (all-defined-out))

(defparam plot2d-tick-skip (integer>=/c 1) 2)

(defproc (default-2d-x-ticks-fun [x-min real?] [x-max real?]) (listof tick?)
  (linear-ticks (plot2d-tick-skip) x-min x-max))

(defproc (default-2d-y-ticks-fun [y-min real?] [y-max real?]) (listof tick?)
  (linear-ticks (plot2d-tick-skip) y-min y-max))

(defproc (default-2d-ticks-fun [x-min real?] [x-max real?] [y-min real?] [y-max real?]
           ) (values (listof tick?) (listof tick?))
  (values (default-2d-x-ticks-fun x-min x-max)
          (default-2d-y-ticks-fun y-min y-max)))
