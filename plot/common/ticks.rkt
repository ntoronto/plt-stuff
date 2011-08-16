#lang racket/base

(require racket/string racket/list racket/contract
         "math.rkt" "list.rkt" "contract.rkt")

(provide (all-defined-out))

(struct tick (p label major?) #:transparent)

(define (tick-ps->majors ps major-skip)
  (define zero-idx (list-index 0 ps =))
  (define zero-idx-rem (if (zero-idx . < . 0) 0 (remainder zero-idx major-skip)))
  (for/list ([n  (in-range (length ps))])
    (= (remainder n major-skip) zero-idx-rem)))

(define ((default-range->ticks major-skip) x-min x-max)
  (when (x-min . >= . x-max)
    (error 'default-range->ticks
           "expected x-min < x-max; got x-min = ~e and x-max = ~e" x-min x-max))
  (let ([x-min  (inexact->exact x-min)]
        [x-max  (inexact->exact x-max)])
    (define e (floor-log10 (- x-max x-min)))
    (define mag (expt 10 e))
    (define step
      (let ([y  (/ (- x-max x-min) mag)])
        (cond [(y . < . 2)   (* 1/5 mag)]
              [(y . < . 5)   (* 1/2 mag)]
              [(y . < . 10)  mag])))
    (define start (* (ceiling (/ x-min step)) step))
    (define stop (* (floor (/ x-max step)) step))
    (define num (+ 1 (round (/ (- stop start) step))))
    (define ps (real-seq start stop num))
    (define labels (map real->tick-label ps))
    (define majors (tick-ps->majors ps major-skip))
    (map tick ps labels majors)))

(define (real->tick-label x)
  (cond [(zero? x)  "0"]
        [else
         (define e (ceiling-log10 x))
         (cond [(e . > . 4)
                (format "~ae~a" (real->string/trunc (/ x (expt 10 e)) 15) e)]
               [(e . < . -4)
                (format "~ae~a" (real->string/trunc (/ x (expt 10 e)) 15) e)]
               [else            (real->string/trunc x 15)])]))

(define (any->tick-label a)
  (cond [(string? a)   a]
        [(symbol? a)   (symbol->string a)]
        [(real? a)     (real->tick-label a)]
        [(list? a)     (string-append "("
                                      (string-join (map any->tick-label a) " ")
                                      ")")]
        [(cons? a)     (string-append "(" (any->tick-label (car a))
                                      " . "
                                      (any->tick-label (cdr a)) ")")]
        [(boolean? a)  (if a "true" "false")]
        [(char? a)     (list->string (list a))]
        [else  (error 'any->tick-label "cannot render as tick label: ~e" a)]))
