#lang racket/base

(require racket/contract
         "../common/contract.rkt"
         "../common/transform.rkt"
         "../common/sample.rkt")

(provide (all-defined-out))

(defparam plot2d-x-transform (real? real? . -> . invertible-fun?) id-transform)
(defparam plot2d-y-transform (real? real? . -> . invertible-fun?) id-transform)

(define function->sampler (make-function->sampler plot2d-x-transform))
(define inverse->sampler (make-function->sampler plot2d-y-transform))
(define 2d-function->sampler (make-2d-function->sampler plot2d-x-transform plot2d-y-transform))
