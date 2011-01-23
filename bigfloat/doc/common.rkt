#lang racket/base

(require (for-syntax racket/base)
         scribble/eval
         scribble/core
         (for-label typed/racket/base
                    "../bigfloat-typed.rkt"))

(provide make-bigfloat-eval
         bfexamples
         (for-label (all-from-out typed/racket/base
                                  "../bigfloat-typed.rkt")))
(define (make-bigfloat-eval)
  (define eval (make-base-eval))
  (eval '(require typed/racket/base))
  (eval '(require "../bigfloat-typed.rkt"))
  (eval '(require "maybe-rename.rkt"))
  (λ (v)
    (cond [(syntax? v)  (eval #`(maybe-rename #,v))]
          [(list? v)  (eval `(maybe-rename ,v))]
          [else  (eval v)])))

(define bigfloat-eval (make-bigfloat-eval))

; the actual style is in manual-style.css - this just names it
(define example-style
  (style "examples" '()))

(define-syntax (bfexamples stx)
  (syntax-case stx ()
    [(_ e ...)
     (syntax/loc stx
       (let ([t  (begin (bigfloat-eval '(bf-bits 128))
                        (examples #:eval bigfloat-eval e ...))])
         (table example-style (table-blockss t))))]))
