#lang racket

(require "../bigfloat.rkt"
         (rename-in "../private/utils-syntax.rkt" [cond* cond] [def def]))

(provide (all-defined-out) cond def)

(define (format-ulp-error u)
  (cond [(= u +inf.0)  "+inf.0"]
        [(= u -inf.0)  "-inf.0"]
        [(= u +nan.0)  "+nan.0"]
        [else  (real->decimal-string u 5)]))

(define (display-ulp-results results [threshold 1/2] [num-top 5])
  (def lst (sort results > #:key car))
  (def num (length lst))
  (def us (map (compose exact->inexact car) lst))
  (def num-failed (length (filter (λ (u) (u . > . threshold)) us)))
  (when (not (zero? num-failed))
    (printf "~a/~a failing tests; top ~a:~n"
            num-failed num (min num-top num-failed))
    (let loop ([lst lst] [n num-top])
      (cond [(or (zero? n) (empty? lst))  (void)]
            #:with (def (list u op) (first lst))
            [(u . > . threshold)  (printf "~a~n => ~a ulps error~n"
                                          op (format-ulp-error u))
                                  (loop (rest lst) (sub1 n))]
            [else  (loop (rest lst) n)]))
    (when (num-failed . > . num-top)
      (printf "...~n")))
  (printf "min: ~a, max: ~a, avg: ~a~n"
          (format-ulp-error (apply min us))
          (format-ulp-error (apply max us))
          (format-ulp-error (/ (apply + us) num)))
  (printf "~n"))

(define-syntax (readable-expr stx)
  (syntax-case stx ()
    [(_ (e ...))
     (syntax/loc stx
       `(,(readable-expr e) ...))]
    [(_ e)
     (syntax/loc stx
       (let ([v e])
         (if (procedure? v) 'e v)))]))

(define-syntax (readable-expr/fields stx)
  (syntax-case stx ()
    [(_ (e ...))
     (syntax/loc stx
       `(,(readable-expr/fields e) ...))]
    [(_ e)
     (syntax/loc stx
       (let ([v e])
         (cond [(procedure? v) 'e]
               [(bigfloat? v)  `(bf ,(bigfloat-fields->string v))]
               [else  v])))]))
