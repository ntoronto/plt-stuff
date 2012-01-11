#lang racket

(require rackunit
         "limit-monad.rkt")

;; =============================================================================
;; Helper functions

;; "return" is too much to type
(define r return)

;; partial-sums : (m Q) => (m Q)
(define (partial-sums xs)
  (define (ys n) (if (zero? n) (xs 0) (+ (xs n) (ys (sub1 n)))))
  ys)

;; =============================================================================
;; Exponential function

;; fact : ω => ω
(define (fact n)
  (if (zero? n) 1 (* n (fact (sub1 n)))))

;; exp-seq : Q => m Q
(define (exp-seq x)
  (partial-sums (λ (n) (/ (expt x n) (fact n)))))

;; exp-lim : m Q => m Q
(define (rexp xs)
  (bind xs exp-seq))

;; exp 1 correct to 200 digits in ~2ms
(check-equal? 
 (real->decimal-string
  (time ((rexp (r 1)) 120))
  200)
 (string-append "2."
                "71828182845904523536028747135266249775724709369995"
                "95749669676277240766303535475945713821785251664274"
                "27466391932003059921817413596629043572900334295260"
                "59563073813232862794349076323382988075319525101901"))

;; =============================================================================
;; Arithmetic, arctangent, and pi

;; These are continuous and defined everywhere, except '/' when denominator
;; is zero
(define r+ (lift2 +))
(define r- (lift2 -))
(define r* (lift2 *))
(define r/ (lift2 /))

;; atan-seq : Q => m Q
;; Assumes x is in (0,1]
(define (atan-seq x)
  (partial-sums
   (λ (n) (/ (* (expt -1 n) (expt x (add1 (* n 2))))
             (add1 (* n 2))))))

;; ratan : m Q => m Q
(define (ratan xs)
  (bind xs atan-seq))

;; John Machin's formula for pi (ca. 1706)
(define rpi/machin
  (r- (r* (r 16) (ratan (r 1/5)))
      (r* (r 4) (ratan (r 1/239)))))

;; pi correct to 200 digits in ~70ms
(check-equal?
 (real->decimal-string
  (time (rpi/machin 141))
  200)
 (string-append "3."
                "14159265358979323846264338327950288419716939937510"
                "58209749445923078164062862089986280348253421170679"
                "82148086513282306647093844609550582231725359408128"
                "48111745028410270193852110555964462294895493038196"))

;; =============================================================================
;; Pi using Ramanujan's formula

(define ramanujan-sum
  (partial-sums (λ (n) (/ (* (fact (* 4 n)) (+ 1103 (* 26390 n)))
                          (* (expt (fact n) 4) (expt 396 (* 4 n)))))))

(define (rsqrt-2 n)
  (/ (integer-sqrt (* 2 (expt 10 (* 2 n))))
     (expt 10 n)))

;; rsqrt-2 adds 1 digit every index; the Ramanujan sum adds 8, so we slow it
;; down using accel 1/8
(define rpi/ramanujan
  (r/ (r 1) (r* (r/ (r* (r 2) rsqrt-2)
                    (r 9801))
                (accel 1/8 ramanujan-sum))))

;; pi correct to 200 digits, ~1ms
(check-equal?
 (real->decimal-string
  (time (rpi/ramanujan 200))
  200)
 (string-append "3."
                "14159265358979323846264338327950288419716939937510"
                "58209749445923078164062862089986280348253421170679"
                "82148086513282306647093844609550582231725359408128"
                "48111745028410270193852110555964462294895493038196"))
