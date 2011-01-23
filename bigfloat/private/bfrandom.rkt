#lang typed/racket/base

(require "utils.rkt" "bf.rkt")

(provide bfrandom bfrandom-signed)

;; We want this as high as Racket will let it go to keep from shortening the
;; PRNG's period too much
(define random-chunk-bits 31)

(: random-integer (-> Integer))
(define (random-integer)
  (def k (2^ random-chunk-bits))
  (let loop ([res  0] [bits (bf-bits)])
    (cond [(zero? bits)  res]
          [(bits . >= . random-chunk-bits)
           (loop (+ (res . << . random-chunk-bits)
                    (random k))
                 (- bits random-chunk-bits))]
          [else  (+ (res . << . bits) (random (2^ bits)))])))

(: bfrandom (-> bigfloat))
(define (bfrandom)
  (new-bigfloat (random-integer) (- (bf-bits))))

(: bfrandom-signed (-> bigfloat))
(define (bfrandom-signed)
  (def i (random-integer))
  (new-bigfloat (if ((random) . > . 0.5) (- i) i) (- (bf-bits))))
