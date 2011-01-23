#lang typed/racket/base

(require #;"../bigfloat.rkt" "../private/utils.rkt")

(require/typed
 rackunit
 [check-equal? (case-lambda
                 (Any Any -> Void)
                 (Any Any String -> Void))])

(check-equal? (number->string (<<r #b11 -1) 2) "10")
(check-equal? (number->string (<<r #b111000 -3) 2) "111")
(check-equal? (number->string (<<r #b111100 -3) 2) "1000")
(check-equal? (number->string (<<r #b111101 -3) 2) "1000")
(check-equal? (number->string (<<r #b110000 -3) 2) "110")
(check-equal? (number->string (<<r #b110100 -3) 2) "110")
(check-equal? (number->string (<<r #b110101 -3) 2) "111")

(let-values ([(x shift)  (rounding-shift #b11 -1)])
  (check-equal? (number->string x 2) "10")
  (check-equal? shift -1))

(let-values ([(x shift)  (rounding-shift #b11111 -1)])
  (check-equal? (number->string x 2) "10000")
  (check-equal? shift -1))

(check-equal? (number->string (<<r #b-11 -1) 2) "-10")
(check-equal? (number->string (<<r #b-111000 -3) 2) "-111")
(check-equal? (number->string (<<r #b-111100 -3) 2) "-1000")
(check-equal? (number->string (<<r #b-111101 -3) 2) "-1000")
(check-equal? (number->string (<<r #b-110000 -3) 2) "-110")
(check-equal? (number->string (<<r #b-110100 -3) 2) "-110")
(check-equal? (number->string (<<r #b-110101 -3) 2) "-111")

(let-values ([(x shift)  (rounding-shift #b-11 -1)])
  (check-equal? (number->string x 2) "-10")
  (check-equal? shift -1))

(let-values ([(x shift)  (rounding-shift #b-11111 -1)])
  (check-equal? (number->string x 2) "-10000")
  (check-equal? shift -1))
