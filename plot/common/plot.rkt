#lang racket/base

(require racket/contract "contract.rkt")

(provide (all-defined-out))

(defparam plot-width (integer>=/c 1) 400)
(defparam plot-height (integer>=/c 1) 400)
(defparam plot-new-window? boolean? #f)
(defparam plot-jpeg-quality (integer-in 0 100) 90)
(defparam plot-ps-interactive? boolean? #f)
(defparam plot-pdf-interactive? boolean? #f)
