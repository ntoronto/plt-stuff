#lang racket

(require rackunit)

#|
The computable limit monad

Computations in the computable limit monad have type

    m a = (ω => a).

where ω is the set of finite ordinals, for some type 'a' represeting a metric
space. Not expressed in the type is the fact that the (lazy) sequences should
converge.

Computations cannot be run, but they can be evaluated at arbitarily high
indexes to get answers with arbitrarily small error.
|#

;; partial-sums : (m Q) => (m Q)
(define (partial-sums xs)
  (define (ys n) (if (zero? n) (xs 0) (+ (xs n) (ys (sub1 n)))))
  ys)

;; return : a => m a
(define ((r c) n) c)

;; lift : (a => b) => (m a => m b)
;; Proof obligations: f is continuous at (limit xs)
;;                    f is defined for all xs
(define ((lift f) xs)  (compose f xs))

;; join : m (m a) => m a
;; Proof obligation: (flip xss) converges uniformly
(define ((join xss) n)  ((xss n) n))

;; bind : m a => (a => m b) => m b
;; Proof obligations: f is continuous at (limit xs)
;;                    f is defined for all xs
;;                    flip (f ∘ xs) converges uniformly
(define (bind xs f)  (join ((lift f) xs)))

;; lift2 : (a => b => c) => m a => m b => m c
;; Proof obligations: f is continuous at limit (xs, ys)
(define ((lift2 f) xs ys)
  (bind xs (λ (x) (bind ys (λ (y) (r (f x y)))))))

(define limit join)

;; accel : Q => m a => m a
;; Adjusts the speed of convergence; see rpi-ramanujan for example of use
(define ((accel m xs) n)
  (xs (floor (* m n))))

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

;; exp 1 correct to 200 digits, ~7ms
(check-equal? 
 (real->decimal-string
  (time ((rexp (r 1)) 120))
  200)
 (string-append "2."
                "718281828459045235360287471352662497757247093699959574966967"
                "627724076630353547594571382178525166427427466391932003059921"
                "817413596629043572900334295260595630738132328627943490763233"
                "82988075319525101901"))

;; =============================================================================
;; Arithmetic, arctangent, and pi

;; All these are continuous everywhere, except '/' when denominator is zero
(define r+ (lift2 +))
(define r- (lift2 -))
(define rneg (lift -))
(define r* (lift2 *))
(define rsqr (lift sqr))
(define r/ (lift2 /))
(define rinv (lift /))

;; atan-seq : Q => m Q
(define (atan-seq x)
  (cond [(zero? x)    (r 0)]
        ; Range reduction techniques:
        [(x . < . 0)  (rneg (atan-seq (- x)))]
        [(x . > . 1)  (r/ (r+ (atan-seq (/ (* 2 x) (- 1 (sqr x))))
                              rpi-machin)
                          (r 2))]
        ; Taylor series expansion:
        [else         (partial-sums
                       (λ (n) (/ (* (expt -1 n) (expt x (add1 (* n 2))))
                                 (add1 (* n 2)))))]))

;; ratan : m Q => m Q
(define (ratan xs)
  (bind xs atan-seq))

;; John Machin's formula for pi (ca. 1706)
(define rpi-machin
  (r- (r* (r 16) (ratan (r 1/5)))
      (r* (r 4) (ratan (r 1/239)))))

;; pi correct to 200 digits, ~33ms
(check-equal?
 (real->decimal-string
  (time (rpi-machin 141))
  200)
 (string-append "3."
                "141592653589793238462643383279502884197169399375105820974944"
                "592307816406286208998628034825342117067982148086513282306647"
                "093844609550582231725359408128481117450284102701938521105559"
                "64462294895493038196"))

;; Uses the fact that limit_{n -> ∞} atan(n) = pi/2
(define rpi-atan-limit
  (r* (r 2) (limit (λ (n) (ratan (rsqr (r n)))))))

;; pi correct to 4 digits, ~240ms (very slow, but it works!)
(check-equal?
 "3.1415"
 (real->decimal-string
  (time (rpi-atan-limit 141))
  4))

;; =============================================================================
;; Sqrt

;; wait-for : (a => boolean) => m a => m b => m b
;; Waits for an element of xs for which pred? is true, then invokes ys with it
;; Allows relaxing the proof obligation for lift and bind: f only needs to be
;; defined on a subsequence of xs (those elements for which pred? is true)
(define (wait-for pred? xs ys)
  (define (g n)
    (if (pred? (xs n)) (ys n) (g (add1 n))))
  g)

;; rsqrt-seq : Q => m Q
;; Adds a decimal digit of precision for every index
;; Newton's method converges quickly, but creates HUGE rational numbers; this
;; plays more nicely with other limits
(define ((rsqrt-seq x) n)
  (define s (expt 10 (* 2 n)))
  (/ (integer-sqrt (* s (numerator x)))
     (integer-sqrt (* s (denominator x)))))

;; rsqrt : m Q => m Q
(define (rsqrt xs)
  (wait-for (λ (x) (x . >= . 0)) xs
            (bind xs rsqrt-seq)))

;; Oscillates above and below zero, but converges to zero; used to test wait-for
(define (r0 n)
  (* (expt -1 n) (/ 1 (expt 10 n))))

;; sqrt 0 correct to 200 digits, ~0ms
(check-equal?
 (real->decimal-string
  (time ((rsqrt r0) 401))
  200)
 (apply string-append "0." (build-list 200 (λ _ "0"))))

;; sqrt 2 correct to 200 digits, ~1ms
(check-equal?
 (real->decimal-string
  (time ((rsqrt (r 2)) 200))
  200)
 (string-append "1."
                "4142135623730950488016887242096980785696718753769480731766797"
                "3799073247846210703885038753432764157273501384623091229702492"
                "4836055850737212644121497099935831413222665927505592755799950"
                "50115278206057147"))

;; =============================================================================
;; Pi using Ramanujan's formula

(define ramanujan-sum
  (partial-sums (λ (n) (/ (* (fact (* 4 n)) (+ 1103 (* 26390 n)))
                          (* (expt (fact n) 4) (expt 396 (* 4 n)))))))

;; rsqrt adds 1 digit every index; the Ramanujan sum adds 8, so we slow it down
;; using accel 1/8
(define rpi-ramanujan
  (rinv (r* (r/ (r* (r 2) (rsqrt (r 2)))
                (r 9801))
            (accel 1/8 ramanujan-sum))))

;; pi correct to 200 digits, ~3ms
(check-equal?
 (real->decimal-string
  (time (rpi-ramanujan 200))
  200)
 (string-append "3."
                "141592653589793238462643383279502884197169399375105820974944"
                "592307816406286208998628034825342117067982148086513282306647"
                "093844609550582231725359408128481117450284102701938521105559"
                "64462294895493038196"))
