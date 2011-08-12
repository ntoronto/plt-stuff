#lang racket/base

(require racket/list)

(provide (all-defined-out))

(define (list-index v lst [equal? equal?])
  (let loop ([lst lst] [idx 0])
    (cond [(null? lst)  -1]
          [(equal? v (car lst))  idx]
          [else  (loop (cdr lst) (add1 idx))])))
