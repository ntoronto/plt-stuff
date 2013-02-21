#lang racket

(require (rename-in "../bigfloat-untyped.rkt"
                    [bfrandom old-bfrandom]
                    [bfrandom-signed old-bfrandom-signed])
         "common.rkt")

(bf-use-small-string-conversion #f)

(define small-bf-bits '(2 8 32 128 512))
(define typical-bf-bits '(2 8 32 128 512 4096 16384))
(define number-of-tests 1000)

(define-syntax (bfrandom-test stx)
  (syntax-case stx ()
    [(_ num binds expr)
     (syntax/loc stx
       (begin
         (printf "(let* ~a~n  ~a):  " 'binds 'expr)
         (def results
           (for/list ([i  (in-range num)])
             (when (not (= (floor (/ i (/ num 20)))
                           (floor (/ (sub1 i) (/ num 20)))))
               (printf "*"))
             (let* binds
               (with-handlers ([exn? (λ (e)
                                       (printf "exception on ~a~n"
                                               (readable-expr expr))
                                       (raise e))])
                 (list (bigfloat->float
                        (bfulp-error
                         expr
                         (parameterize ([bf-bits (* 2 (bf-bits))]) expr)))
                       (readable-expr expr))))))
         (printf "~n")
         (display-ulp-results results)))]))

(define (apply-rational rop . args)
  (apply rop (map (λ (x) (if (bigfloat? x) (bigfloat->rational x) x)) args)))

(define-syntax (exact-random-test stx)
  (syntax-case stx ()
    [(_ num binds expr rexpr)
     (syntax/loc stx
       (begin
         (printf "(let* ~a~n  ~a):  " 'binds 'expr)
         (def results
           (for/list ([i  (in-range num)])
             (when (not (= (floor (/ i (/ num 20)))
                           (floor (/ (sub1 i) (/ num 20)))))
               (printf "*"))
             (let* binds
               (with-handlers ([exn? (λ (e)
                                       (printf "failed on ~a~n"
                                               (readable-expr expr))
                                       (raise e))])
                 (list (exact-ulp-error expr (apply-rational . rexpr))
                       (readable-expr expr))))))
         (printf "~n")
         (display-ulp-results results)))]))

(define (random/rejection thnk blacklist)
  (let loop ([x  (thnk)])
    (if (member x blacklist) (loop (thnk)) x)))

(define (bfrandom #:except [blacklist empty])
  (random/rejection old-bfrandom blacklist))

(define (bfrandom-signed #:except [blacklist empty])
  (random/rejection old-bfrandom-signed blacklist))

(define (make-random/exp thnk exp-min exp-max)
  (λ () ((thnk) . bf*2^ . (+ (random (- exp-max exp-min)) exp-min))))

(define (bfrandom/exp #:except [blacklist empty]
                      #:exp-min [exp-min (- (* 2 (bf-bits)))]
                      #:exp-max [exp-max (* 2 (bf-bits))])
  (random/rejection (make-random/exp bfrandom exp-min exp-max)
                    blacklist))

(define (bfrandom-signed/exp #:except [blacklist empty]
                             #:exp-min [exp-min (- (* 2 (bf-bits)))]
                             #:exp-max [exp-max (* 2 (bf-bits))])
  (random/rejection (make-random/exp bfrandom-signed exp-min exp-max)
                    blacklist))

(define (bfrandom-nearby x #:ulps [ulps 64] #:except [blacklist empty])
  (random/rejection (λ () (bfnext x (- (random (* 2 ulps)) ulps))) blacklist))

(define-syntax-rule (with-random-bf-bits e0 e ...)
  (parameterize ([bf-bits  (+ (random (* 2 (bf-bits))) 2)]) e0 e ...))

(define-syntax-rule (define-exact-unary-test test-bfop
                      bfop rop random-x ...)
  (define (test-bfop)
    (for ([bits  (in-list typical-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a bits~n" 'test-bfop (bf-bits))
        (exact-random-test number-of-tests
                           ([x  (random-x)])
                           (bfop x) (rop x))
        ...))
    (for ([bits  (in-list small-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a output bits, random input bits~n" 'test-bfop (bf-bits))
        (exact-random-test number-of-tests
                           ([x  (with-random-bf-bits (random-x))])
                           (bfop x) (rop x))
        ...))))

(define-syntax-rule (define-exact-binary-test test-bfop
                      bfop rop (random-x random-y) ...)
  (define (test-bfop)
    (for ([bits  (in-list typical-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a bits~n" 'test-bfop (bf-bits))
        (exact-random-test number-of-tests
                           ([x  (random-x)] [y  (random-y x)])
                           (bfop x y) (rop x y))
        ...))
    (for ([bits  (in-list small-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a output bits, random input bits~n" 'test-bfop (bf-bits))
        (exact-random-test number-of-tests
                           ([x  (with-random-bf-bits (random-x))]
                            [y  (with-random-bf-bits (random-y x))])
                           (bfop x y) (rop x y))
        ...))))

(define-syntax-rule (define-bfunary-test test-bfop
                      bfop random-x ...)
  (define (test-bfop)
    (for ([bits  (in-list typical-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a bits~n" 'test-bfop (bf-bits))
        (bfrandom-test number-of-tests
                       ([x  (random-x)])
                       (bfop x))
        ...))
    (for ([bits  (in-list small-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a output bits, random input bits~n" 'test-bfop (bf-bits))
        (bfrandom-test number-of-tests
                       ([x  (with-random-bf-bits (random-x))])
                       (bfop x))
        ...))))

(define-syntax-rule (define-bfbinary-test test-bfop
                      bfop (random-x random-y) ...)
  (define (test-bfop)
    (for ([bits  (in-list typical-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a bits~n" 'test-bfop (bf-bits))
        (bfrandom-test number-of-tests
                       ([x  (random-x)] [y  (random-y x)])
                       (bfop x y))
        ...))
    (for ([bits  (in-list small-bf-bits)])
      (parameterize ([bf-bits bits])
        (printf "----------------------------------------~n")
        (printf "~a, ~a output bits, random input bits~n" 'test-bfop (bf-bits))
        (bfrandom-test number-of-tests
                       ([x  (with-random-bf-bits (random-x))]
                        [y  (with-random-bf-bits (random-y x))])
                       (bfop x y))
        ...))))

;; =============================================================================
;; conversions

;; =============================================================================
;; arithmetic

(define-exact-binary-test test-bf+
  bf+ +
  (bfrandom-signed (λ (_) (bfrandom-signed)))
  (bfrandom-signed/exp (λ (_) (bfrandom-signed/exp)))
  (bfrandom-signed (λ (x) (bfrandom-nearby x)))
  (bfrandom-signed/exp (λ (x) (bfrandom-nearby x))))

(define-exact-binary-test test-bf-
  bf- -
  (bfrandom-signed (λ (_) (bfrandom-signed)))
  (bfrandom-signed/exp (λ (_) (bfrandom-signed/exp)))
  (bfrandom-signed (λ (x) (bfrandom-nearby x)))
  (bfrandom-signed/exp (λ (x) (bfrandom-nearby x))))

(define-exact-binary-test test-bf*
  bf* *
  (bfrandom-signed (λ (_) (bfrandom-signed)))
  (bfrandom-signed/exp (λ (_) (bfrandom-signed/exp)))
  (bfrandom-signed (λ (x) (bfrandom-nearby x)))
  (bfrandom-signed/exp (λ (x) (bfrandom-nearby x))))

(define-exact-binary-test test-bf/
  bf/ /
  (bfrandom-signed (λ (_) (bfrandom-signed #:except (list (bf 0)))))
  (bfrandom-signed/exp (λ (_) (bfrandom-signed/exp #:except (list (bf 0)))))
  (bfrandom-signed (λ (x) (bfrandom-nearby x #:except (list (bf 0)))))
  (bfrandom-signed/exp (λ (x) (bfrandom-nearby x #:except (list (bf 0))))))

(define (exact-quotient x y)
  (truncate (/ x y)))

(define-exact-binary-test test-bfquotient
  bfquotient exact-quotient
  (bfrandom-signed (λ (_) (bfrandom-signed #:except (list (bf 0)))))
  (bfrandom-signed/exp (λ (_) (bfrandom-signed/exp #:except (list (bf 0)))))
  (bfrandom-signed (λ (x) (bfrandom-nearby x #:except (list (bf 0)))))
  (bfrandom-signed/exp (λ (x) (bfrandom-nearby x #:except (list (bf 0))))))

(define (exact-remainder x y)
  (- x (* y (exact-quotient x y))))

(define-exact-binary-test test-bfremainder
  bfremainder exact-remainder
  (bfrandom-signed (λ (_) (bfrandom-signed #:except (list (bf 0)))))
  (bfrandom-signed/exp (λ (_) (bfrandom-signed/exp #:except (list (bf 0)))))
  (bfrandom-signed (λ (x) (bfrandom-nearby x #:except (list (bf 0)))))
  (bfrandom-signed/exp (λ (x) (bfrandom-nearby x #:except (list (bf 0))))))

(define-exact-unary-test test-bfinv
  bfinv /
  (λ () (bfrandom-signed #:except (list (bf 0))))
  (λ () (bfrandom-signed/exp #:except (list (bf 0))))
  (λ () (bfrandom-nearby (bf 1) #:except (list (bf 0)))))

(define-exact-unary-test test-bfadd1
  bfadd1 add1
  bfrandom-signed
  bfrandom-signed/exp)

(define-exact-unary-test test-bfsub1
  bfsub1 sub1
  bfrandom-signed
  bfrandom-signed/exp)

(define-bfunary-test test-bfsqrt
  bfsqrt bfrandom bfrandom/exp)

;; =============================================================================
;; constants

(define-syntax-rule (define-bf-const-test test-bf-const
                      bf-const start stop step)
  (define (test-bf-const)
    (printf "----------------------------------------~n")
    (printf "~a with bits (in-range ~a ~a ~a)~n" 'test-bf-const start stop step)
    (display-ulp-results
     (for/list ([bits  (in-range start stop step)])
       (list (bigfloat->float
              (bfulp-error
               (parameterize ([bf-bits  bits]) (bf-const))
               (parameterize ([bf-bits  (* 2 bits)]) (bf-const))))
             `(parameterize ([bf-bits  ,bits]) (bf-const)))))))

(define-bf-const-test test-bf-pi
  bf-pi 2 65536 1)

;; =============================================================================
;; arithmetic-geometric mean

(define-bfbinary-test test-bfagm
  bfagm
  (bfrandom (λ (_) (bfrandom)))
  (bfrandom/exp (λ (_) (bfrandom/exp))))

;; =============================================================================
;; logarithms

(define-bfunary-test test-bflog
  bflog
  (λ () (bfrandom #:except (list (bf 0))))
  (λ () (bfrandom/exp #:except (list (bf 0)))))

(define-bfbinary-test test-bflogb
  bflogb
  ((λ () (bfrandom #:except (list (bf 0) (bf 1))))
   (λ (_) (bfrandom #:except (list (bf 0)))))
  ((λ () (bfrandom/exp #:except (list (bf 0) (bf 1))))
   (λ (_) (bfrandom/exp #:except (list (bf 0))))))

(define-bf-const-test test-bf-log-2
  bf-log-2 2 65536 1)

(define-bf-const-test test-bf-log-10
  bf-log-10 2 65536 1)

;; =============================================================================
;; exponentials

(define-bfunary-test test-bfexp
  bfexp bfrandom-signed bfrandom-signed/exp)

(define-bfbinary-test test-bfexpt
  bfexpt
  (bfrandom (λ (_) (bfrandom-signed)))
  (bfrandom/exp (λ (_) (bfrandom-signed/exp))))

(define-bf-const-test test-bf-e
  bf-e 2 16384 1)

;; =============================================================================
;; trigonometric

(define (bfrandom-angle)
  (def pi*2 (bf*2 (bf-pi)))
  (bf*2 (bf* (bf-pi) (bfrandom-signed))))

(define-bfunary-test test-bfcos
  bfcos bfrandom-angle bfrandom-signed/exp)

(define-bfunary-test test-bfsin
  bfsin bfrandom-angle bfrandom-signed/exp)

(define-bfunary-test test-bftan
  bftan bfrandom-angle bfrandom-signed/exp)

;; =============================================================================
;; inverse trigonometric

(define-bfunary-test test-bfatan
  bfatan bfrandom-signed bfrandom-signed/exp)

(define-bfunary-test test-bfasin
  bfasin
  bfrandom-signed
  (λ () (bfrandom-signed/exp #:exp-min (- (* 2 (bf-bits))) #:exp-max 0)))

(define-bfunary-test test-bfacos
  bfacos
  bfrandom-signed
  (λ () (bfrandom-signed/exp #:exp-min (- (* 2 (bf-bits))) #:exp-max 0)))

;; =============================================================================
;; hyperbolic

(define-bfunary-test test-bfsinh
  bfsinh
  bfrandom-signed
  bfrandom-signed/exp)

;; =============================================================================
;; inverse hyperbolic

;; =============================================================================
;; distributions?

