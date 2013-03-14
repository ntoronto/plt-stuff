#lang typed/racket/base

(provide Null-Rect null-rect null-rect?)

(require "../untyped-utils.rkt")

(define-singleton-type Null-Rect null-rect)
