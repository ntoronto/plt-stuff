#lang racket/base

(require racket/contract
         "../common/ticks.rkt"
         "../common/contract.rkt")

(provide (all-defined-out))

(defparam plot3d-tick-skip (integer>=/c 1) 2)

(defproc (default-3d-x-ticks-fun [x-min real?] [x-max real?]) (listof tick?)
  (linear-ticks (plot3d-tick-skip) x-min x-max))

(defproc (default-3d-y-ticks-fun [y-min real?] [y-max real?]) (listof tick?)
  (linear-ticks (plot3d-tick-skip) y-min y-max))

(defproc (default-3d-z-ticks-fun [z-min real?] [z-max real?]) (listof tick?)
  (linear-ticks (plot3d-tick-skip) z-min z-max))

(defproc (default-3d-ticks-fun
           [x-min real?] [x-max real?]
           [y-min real?] [y-max real?]
           [z-min real?] [z-max real?]
           ) (values (listof tick?) (listof tick?) (listof tick?))
  (values (default-3d-x-ticks-fun x-min x-max)
          (default-3d-y-ticks-fun y-min y-max)
          (default-3d-z-ticks-fun z-min z-max)))
