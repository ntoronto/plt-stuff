#lang racket/base

(require scribble/eval
         "../plot2d.rkt"
         "../plot3d.rkt"
         (for-label "../plot2d.rkt"
                    "../plot3d.rkt"))

(provide (all-defined-out)
         (all-from-out "../plot2d.rkt"
                       "../plot3d.rkt")
         (for-label (all-from-out "../plot2d.rkt"
                                  "../plot3d.rkt")))

(define (plot-name) "PLoT")

(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval '(require racket/math racket/function))
    (eval '(require "../plot2d.rkt"))
    (eval '(require "../plot3d.rkt"))
    eval))
