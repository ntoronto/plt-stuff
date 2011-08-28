#lang racket

(require syntax/parse)

(define-syntax-class argument-spec
  #:description "argument specification"
  (pattern [name:id contract:expr])
  (pattern [name:id contract:expr default:expr])
  (pattern [kw:keyword name:id contract:expr])
  (pattern [kw:keyword name:id contract:expr default:expr]))

(provide argument-spec)
