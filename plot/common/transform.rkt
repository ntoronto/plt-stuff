#lang racket/base

(require racket/math racket/flonum racket/contract racket/match
         "math.rkt"
         "contract.rkt")

(provide (all-defined-out))

(define-struct/contract invertible-fun ([f (real? . -> . real?)] [finv (real? . -> . real?)])
  #:transparent)

(define (nonlinear-seq x-min x-max samples transform #:start? [start? #t] #:end? [end? #t])
  (match-define (invertible-fun _ finv) (transform x-min x-max))
  (map finv (linear-seq x-min x-max samples #:start? start? #:end? end?)))

;; Turns any total, surjective, monotone flonum op and its inverse into an axis transform
(define ((make-axis-transform flop flinv) x-min x-max)
  (let ([x-min  (exact->inexact x-min)]
        [x-max  (exact->inexact x-max)])
    (define fx-min (flop x-min))
    (define fx-scale (fl/ (fl- x-max x-min)
                          (fl- (flop x-max) fx-min)))
    (define (f x)
      (fl+ x-min (fl* (fl- (flop (exact->inexact x)) fx-min)
                      fx-scale)))
    (define (finv y)
      (flinv (fl+ fx-min (fl/ (fl- (exact->inexact y) x-min)
                              fx-scale))))
    (invertible-fun f finv)))

;; ===================================================================================================
;; Specific axis transforms

(define (sine-diag d)
  (let ([d  (exact->inexact d)])
    (λ (x) (let ([x  (exact->inexact x)])
             (fl+ x (fl* (fl/ 1.0 (fl* 4.0 d)) (flsin (fl* d x))))))))

(define (sine-diag-diff d)
  (let ([d  (exact->inexact d)])
    (λ (x) (let ([x  (exact->inexact x)])
             (fl- (fl/ (flcos (fl* d x)) 4.0) 1.0)))))

(define (sine-diag-inv d)
  (flnewton-invert (sine-diag d) (sine-diag-diff d) values 10))

(define cbrt
  (let ([e  (exact->inexact 1/3)])
    (λ (x) (let ([x  (exact->inexact x)])
             (fl* (sgn x) (expt (flabs x) e))))))

(define (cube x)
  (let ([x  (exact->inexact x)])
    (fl* x (fl* x x))))


(define (id-transform x-min x-max) (invertible-fun values values))

(define (log-transform x-min x-max)
  (when ((exact->inexact x-min) . <= . 0)
    (raise-type-error 'log-transform "positive real" 0 x-min x-max))
  ((make-axis-transform fllog flexp) x-min x-max))

(define exp-transform (make-axis-transform flexp fllog))
(define cbrt-transform (make-axis-transform cbrt cube))
(define cube-transform (make-axis-transform cube cbrt))

(defproc (hand-drawn-x-transform [freq (and/c real? (>/c 0))]) (real? real? . -> . invertible-fun?)
  (λ (mn mx)
    (define d (/ freq (- mx mn)))
    ((make-axis-transform (sine-diag d) (sine-diag-inv d)) mn mx)))
