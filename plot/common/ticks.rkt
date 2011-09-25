#lang racket/base

(require racket/string racket/list racket/contract racket/pretty
         "math.rkt"
         "utils.rkt"
         "contract.rkt")

(provide (all-defined-out))

(define-struct/contract tick
  ([p real?] [label string?] [major? boolean?])
  #:transparent)

(define (tick-ps->majors ps major-skip)
  (define zero-idx (list-index 0 ps =))
  (define zero-idx-rem (if (zero-idx . < . 0) 0 (remainder zero-idx major-skip)))
  (for/list ([n  (in-range (length ps))])
    (= (remainder n major-skip) zero-idx-rem)))

(define (linear-ticks major-skip x-min x-max)
  (when (x-min . >= . x-max)
    (error 'default-range->ticks "expected x-min < x-max; got x-min = ~e and x-max = ~e" x-min x-max))
  (let ([x-min  (inexact->exact x-min)]
        [x-max  (inexact->exact x-max)])
    (define e (floor-log10 (- x-max x-min)))
    (define mag (expt 10 e))
    (define step (let ([y  (/ (- x-max x-min) mag)])
                   (cond [(y . < . 2)   (* 1/5 mag)]
                         [(y . < . 5)   (* 1/2 mag)]
                         [(y . < . 10)  mag])))
    (define start (* (ceiling (/ x-min step)) step))
    (define stop (* (floor (/ x-max step)) step))
    (define num (+ 1 (round (/ (- stop start) step))))
    (define ps (linear-seq start stop num))
    (define digits (digits-for-range x-min x-max))
    (define labels (map (λ (p) (real->string/trunc p digits)) ps))
    (define majors (tick-ps->majors ps major-skip))
    (map tick ps labels majors)))

(define (any->tick-label a [digits 7])
  (let loop ([a a])
    (cond [(string? a)  a]
          [(symbol? a)  (symbol->string a)]
          [(real? a)    (real->string/trunc a digits)]
          [(list? a)    (string-append "(" (string-join (map loop a) " ") ")")]
          [(cons? a)    (string-append "(" (loop (car a)) " . " (loop (cdr a)) ")")]
          [(boolean? a) (if a "true" "false")]
          [(char? a)    (list->string (list a))]
          [else  (pretty-format a)])))
