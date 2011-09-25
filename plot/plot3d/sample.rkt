#lang racket/base

(require racket/contract
         "../common/contract.rkt"
         "../common/transform.rkt"
         "../common/sample.rkt")

(provide (all-defined-out))

(defparam plot3d-x-transform (real? real? . -> . invertible-fun?) id-transform)
(defparam plot3d-y-transform (real? real? . -> . invertible-fun?) id-transform)
(defparam plot3d-z-transform (real? real? . -> . invertible-fun?) id-transform)

(define 2d-function->sampler (make-2d-function->sampler plot3d-x-transform plot3d-y-transform))
(define 3d-function->sampler
  (make-3d-function->sampler plot3d-x-transform plot3d-y-transform plot3d-z-transform))
