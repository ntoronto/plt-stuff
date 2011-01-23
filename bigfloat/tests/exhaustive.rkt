#lang racket

(require "../bigfloat.rkt" "common.rkt")

#;; all 1236544 tests pass
(let ()
  (printf "Testing bf= bf> bf>=~n")
  (for*/fold ([res #t] [num 0])
    ([x-bits  (in-range 2 6)]
     [x-exp   (in-range (- x-bits) (add1 x-bits))]
     [x-sig   (in-range (- 1 (expt 2 x-bits)) (expt 2 x-bits))]
     [y-bits  (in-range 2 6)]
     [y-exp   (in-range (- y-bits) (add1 y-bits))]
     [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))])
    (def x (make-bigfloat x-sig x-exp))
    (def y (make-bigfloat y-sig y-exp))
    (def rx (bigfloat->rational x))
    (def ry (bigfloat->rational y))
    (values (and (equal? (bf= x y) (= rx ry))
                 (equal? (bf> x y) (> rx ry))
                 (equal? (bf>= x y) (>= rx ry))
                 res)
            (add1 num))))

#;; all 1053366 tests pass
(let ()
  (printf "Testing bf+ and bf-~n")
  (def results
    (for*/fold ([res empty])
      ([x-bits  (in-range 2 5)]
       [x-exp   (in-range (- x-bits) (add1 x-bits))]
       [x-sig   (in-range (- 1 (expt 2 x-bits)) (expt 2 x-bits))]
       [y-bits  (in-range 2 5)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       [bits    (in-range 2 5)])
      (parameterize ([bf-bits bits])
        (def x (make-bigfloat x-sig x-exp))
        (def y (make-bigfloat y-sig y-exp))
        (def rx (bigfloat->rational x))
        (def ry (bigfloat->rational y))
        (list* (list (exact-ulp-error (bf+ x y) (+ rx ry))
                     (readable-expr/fields (bf+ x y)))
               (list (exact-ulp-error (bf- x y) (- rx ry))
                          (readable-expr/fields (bf- x y)))
               res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 79056 tests pass
(let ()
  (printf "Testing bfadd1 bfsub1~n")
  (def results
    (for*/fold ([res empty])
      ([y-bits  (in-range 2 8)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       [bits    (in-range 2 8)])
      (parameterize ([bf-bits bits])
        (def y (make-bigfloat y-sig y-exp))
        (def ry (bigfloat->rational y))
        (list* (list (exact-ulp-error (bfadd1 y) (add1 ry))
                     (readable-expr/fields (bfadd1 y)))
               (list (exact-ulp-error (bfsub1 y) (sub1 ry))
                     (readable-expr/fields (bfsub1 y)))
               res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 526683 tests pass
(let ()
  (printf "Testing bf*~n")
  (def results
    (for*/fold ([res empty])
      ([x-bits  (in-range 2 5)]
       [x-exp   (in-range (- x-bits) (add1 x-bits))]
       [x-sig   (in-range (- 1 (expt 2 x-bits)) (expt 2 x-bits))]
       [y-bits  (in-range 2 5)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       [bits    (in-range 2 5)])
      (parameterize ([bf-bits bits])
        (def x (make-bigfloat x-sig x-exp))
        (def y (make-bigfloat y-sig y-exp))
        (def rx (bigfloat->rational x))
        (def ry (bigfloat->rational y))
        (cons (list (exact-ulp-error (bf* x y) (* rx ry))
                    (readable-expr/fields (bf* x y)))
              res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

(define (sqr* r k)
  (if (zero? k) r (sqr* (sqr r) (sub1 k))))

#;; all 139232 tests pass
(let ()
  (printf "Testing bfsqrt~n")
  (def results
    (for*/fold ([res empty])
      ([y-bits  (in-range 2 10)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range 0 (expt 2 y-bits))]
       [bits    (in-range 2 10)])
      (parameterize ([bf-bits bits])
        (def y (make-bigfloat y-sig y-exp))
        (def ry (bigfloat->rational y))
        (cons (list (bigfloat->float
                     (bfulp-error (bfsqrt y)
                                  (parameterize ([bf-bits (* 2 (bf-bits))])
                                    (bfsqrt y))))
                    (readable-expr/fields (bfsqrt y)))
              res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 500286 tests pass
(let ()
  (printf "Testing bf/~n")
  (def results
    (for*/fold ([res empty])
      ([x-bits  (in-range 2 5)]
       [x-exp   (in-range (- x-bits) (add1 x-bits))]
       [x-sig   (in-range (- 1 (expt 2 x-bits)) (expt 2 x-bits))]
       [y-bits  (in-range 2 5)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       #:when (not (zero? y-sig))
       [bits    (in-range 2 5)])
      (parameterize ([bf-bits bits])
        (def x (make-bigfloat x-sig x-exp))
        (def y (make-bigfloat y-sig y-exp))
        (def rx (bigfloat->rational x))
        (def ry (bigfloat->rational y))
        (cons (list (exact-ulp-error (bf/ x y) (/ rx ry))
                    (readable-expr/fields (bf/ x y)))
              res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 39168 tests pass
(let ()
  (printf "Testing bfinv~n")
  (def results
    (for*/fold ([res empty])
      ([y-bits  (in-range 2 8)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       #:when (not (zero? y-sig))
       [bits    (in-range 2 8)])
      (parameterize ([bf-bits bits])
        (def y (make-bigfloat y-sig y-exp))
        (def ry (bigfloat->rational y))
        (cons (list (exact-ulp-error (bfinv y) (/ ry))
                    (readable-expr/fields (bfinv y)))
              res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 500286 tests pass
(let ()
  (printf "Testing bfquotient~n")
  (def results
    (for*/fold ([res empty])
      ([x-bits  (in-range 2 5)]
       [x-exp   (in-range (- x-bits) (add1 x-bits))]
       [x-sig   (in-range (- 1 (expt 2 x-bits)) (expt 2 x-bits))]
       [y-bits  (in-range 2 5)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       #:when (not (zero? y-sig))
       [bits    (in-range 2 5)])
      (parameterize ([bf-bits bits])
        (def x (make-bigfloat x-sig x-exp))
        (def y (make-bigfloat y-sig y-exp))
        (def rx (bigfloat->rational x))
        (def ry (bigfloat->rational y))
        (cons (list (exact-ulp-error (bfquotient x y) (truncate (/ rx ry)))
                    (readable-expr/fields (bfquotient x y)))
              res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 500286 tests pass
(let ()
  (printf "Testing bfexact-quotient~n")
  (def results
    (for*/fold ([res empty])
      ([x-bits  (in-range 2 5)]
       [x-exp   (in-range (- x-bits) (add1 x-bits))]
       [x-sig   (in-range (- 1 (expt 2 x-bits)) (expt 2 x-bits))]
       [y-bits  (in-range 2 5)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       #:when (not (zero? y-sig))
       [bits    (in-range 2 5)])
      (parameterize ([bf-bits bits])
        (def x (make-bigfloat x-sig x-exp))
        (def y (make-bigfloat y-sig y-exp))
        (def rx (bigfloat->rational x))
        (def ry (bigfloat->rational y))
        (cons (list (exact-ulp-error (make-bigfloat (bfexact-quotient x y) 0)
                                     (truncate (/ rx ry)))
                    (readable-expr/fields (bfexact-quotient x y)))
              res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 1110784 tests pass
(let ()
  (printf "Testing bftruncate bffloor bfceiling bfround~n")
  (def results
    (for*/fold ([res empty])
      ([y-bits  (in-range 2 10)]
       [y-exp   (in-range (- y-bits) (add1 y-bits))]
       [y-sig   (in-range (- 1 (expt 2 y-bits)) (expt 2 y-bits))]
       [bits    (in-range 2 10)])
      (parameterize ([bf-bits bits])
        (def y (make-bigfloat y-sig y-exp))
        (def ry (bigfloat->rational y))
        (list* (list (exact-ulp-error (bftruncate y) (truncate ry))
                     (readable-expr/fields (bftruncate y)))
               (list (exact-ulp-error (bffloor y) (floor ry))
                     (readable-expr/fields (bffloor y)))
               (list (exact-ulp-error (bfceiling y) (ceiling ry))
                     (readable-expr/fields (bfceiling y)))
               (list (exact-ulp-error (bfround y) (round ry))
                     (readable-expr/fields (bfround y)))
               res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))

#;; all 259992 tests pass
(let ()
  (printf "Testing bf-pi bf-log-2 bf-log-10 bf-e~n")
  (def results
    (for*/fold ([res empty]) ([const  (list bf-pi bf-log-2 bf-log-10 bf-e)]
                              [bits  (in-range 2 65000)])
      ;(printf "const = ~a, bits = ~a~n" const bits)
      (parameterize ([bf-bits bits])
        (cons (list (bigfloat->float
                     (bfulp-error (const)
                                  (parameterize ([bf-bits (+ 20 (bf-bits))])
                                    (const))))
                    `(,const))
              res))))
  (printf "~a tests~n" (length results))
  (display-ulp-results results))
