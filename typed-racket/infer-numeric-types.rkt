#lang racket

(require math/flonum
         math/base)

(module typed-defs typed/racket/base
  (provide (all-defined-out))
  (define-predicate single-float-complex? Single-Flonum-Complex))

(require 'typed-defs)

;; ===================================================================================================
;; Numeric types

;; These need to be disjoint
(define type-list
  (list 'Zero
        'PosInt
        'NegInt
        'FlonumPosZero          ; +0.0
        'FlonumNegZero          ; -0.0
        'NegFlonumNoNan
        'PosFlonumNoNan
        'FlonumNan
        'SingleFlonumPosZero    ; +0.0f0
        'SingleFlonumNegZero    ; -0.0f0
        'SingleNegFlonumNoNan
        'SinglePosFlonumNoNan
        'SingleFlonumNan
        'NegRatNotInt           ; Negative-Rational - Integer
        'PosRatNotInt           ; Positive-Rational - Integer
        'FloatComplex           ; Both parts flonum
        'SingleFloatComplex     ; Both parts single-flonum
        'ExactComplex           ; Both parts exact
        'MixedComplex           ; None of the above three
        'Nothing))

(define types (list->set type-list))

;; A type is a symbol (a basic type), a set of types (a union), or a `fun-type'

(struct fun-type (arg res) #:transparent)
;; An argument type is a type (unary function), or a list of two types (binary function)

(define (type-union t1 t2)
  (cond [(not (set? t1))  (type-union (set t1) t2)]
        [(not (set? t2))  (type-union t1 (set t2))]
        [else  (set-union t1 t2)]))

(define (exact-integer-type x)
  (cond [(x . > . 0)  'PosInt]
        [(x . < . 0)  'NegInt]
        [else  'Zero]))

(define (flonum-type x)
  (cond [(eqv? x +0.0)  'FlonumPosZero]
        [(eqv? x -0.0)  'FlonumNegZero]
        [(x . < . 0.0)  'NegFlonumNoNan]
        [(x . > . 0.0)  'PosFlonumNoNan]
        [else  'FlonumNan]))

(define (single-flonum-type x)
  (cond [(eqv? x +0.0f0)  'SingleFlonumPosZero]
        [(eqv? x -0.0f0)  'SingleFlonumNegZero]
        [(x . < . 0.0f0)  'SingleNegFlonumNoNan]
        [(x . > . 0.0f0)  'SinglePosFlonumNoNan]
        [else  'SingleFlonumNan]))

(define (real-type x)
  (cond [(flonum? x)  (flonum-type x)]
        [(single-flonum? x)  (single-flonum-type x)]
        [(x . < . 0)  'NegRatNotInt]
        [else  'PosRatNotInt]))

(define (complex-type x)
  (cond [(float-complex? x)  'FloatComplex]
        [(single-float-complex? x)  'SingleFloatComplex]
        [(exact? x)  'ExactComplex]
        [else  'MixedComplex]))

(define (value-type x)
  (cond [(equal? x 'bottom)  'Nothing]
        [(exact-integer? x)  (exact-integer-type x)]
        [(real? x)  (real-type x)]
        [else  (complex-type x)]))

;; ===================================================================================================
;; Simplifying and compressing types

(define simplify-type-table
  (list (cons (set 'Zero 'PosInt) 'NonnegInt)
        (cons (set 'Zero 'NegInt) 'NonposInt)
        (cons (set 'NonnegInt 'NegInt) 'Int)
        (cons (set 'PosInt 'NegInt) 'NonzeroInt)
        
        (cons (set 'NegRatNotInt 'PosRatNotInt) 'RatNotInt)
        (cons (set 'RatNotInt 'Int) 'Rat)
        (cons (set 'NonnegInt 'PosRatNotInt) 'NonnegRat)
        (cons (set 'NonposInt 'NegRatNotInt) 'NonposRat)
        (cons (set 'PosInt 'PosRatNotInt) 'PosRat)
        (cons (set 'NegInt 'NegRatNotInt) 'NegRat)
        (cons (set 'NonzeroInt 'RatNotInt) 'NonzeroRat)
        
        (cons (set 'FlonumPosZero 'FlonumNegZero 'NegFlonumNoNan 'PosFlonumNoNan) 'FlonumNoNan)
        (cons (set 'NegFlonumNoNan 'PosFlonumNoNan) 'NonzeroFlonumNoNan)
        (cons (set 'FlonumNoNan 'FlonumNan) 'Flonum)
        (cons (set 'FlonumPosZero 'FlonumNegZero) 'FlonumZeroNoNan)
        (cons (set 'FlonumZeroNoNan 'PosFlonumNoNan) 'NonnegFlonumNoNan)
        (cons (set 'FlonumZeroNoNan 'NegFlonumNoNan) 'NonposFlonumNoNan)
        (cons (set 'FlonumZeroNoNan 'FlonumNan) 'FlonumZero)
        (cons (set 'NonzeroFlonumNoNan 'FlonumNan) 'NonzeroFlonum)
        (cons (set 'PosFlonumNoNan 'FlonumNan) 'PosFlonum)
        (cons (set 'NegFlonumNoNan 'FlonumNan) 'NegFlonum)
        (cons (set 'NonnegFlonumNoNan 'FlonumNan) 'NonnegFlonum)
        (cons (set 'NonposFlonumNoNan 'FlonumNan) 'NonposFlonum)
        
        (cons (set 'SingleFlonumPosZero
                   'SingleFlonumNegZero
                   'SingleNegFlonumNoNan
                   'SinglePosFlonumNoNan)
              'SingleFlonumNoNan)
        (cons (set 'SingleNegFlonumNoNan 'SinglePosFlonumNoNan) 'SingleNonzeroFlonumNoNan)
        (cons (set 'SingleFlonumNoNan 'SingleFlonumNan) 'SingleFlonum)
        (cons (set 'SingleFlonumPosZero 'SingleFlonumNegZero) 'SingleFlonumZeroNoNan)
        (cons (set 'SingleFlonumZeroNoNan 'SinglePosFlonumNoNan) 'SingleNonnegFlonumNoNan)
        (cons (set 'SingleFlonumZeroNoNan 'SingleNegFlonumNoNan) 'SingleNonposFlonumNoNan)
        (cons (set 'SingleFlonumZeroNoNan 'SingleFlonumNan) 'SingleFlonumZero)
        (cons (set 'SingleNonzeroFlonumNoNan 'SingleFlonumNan) 'SingleNonzeroFlonum)
        (cons (set 'SinglePosFlonumNoNan 'SingleFlonumNan) 'SinglePosFlonum)
        (cons (set 'SingleNegFlonumNoNan 'SingleFlonumNan) 'SingleNegFlonum)
        (cons (set 'SingleNonnegFlonumNoNan 'SingleFlonumNan) 'SingleNonnegFlonum)
        (cons (set 'SingleNonposFlonumNoNan 'SingleFlonumNan) 'SingleNonposFlonum)
        
        (cons (set 'Rat 'Flonum 'SingleFlonum) 'Real)
        
        (cons (set 'FloatComplex 'SingleFloatComplex 'MixedComplex) 'InexactComplex)
        (cons (set 'InexactComplex 'ExactComplex) 'Complex)
        
        (cons (set 'Real 'Complex) 'Number)))

(define unsimplify-type-hash
  (for/hash ([c  (in-list simplify-type-table)])
    (match-define (cons ts t) c)
    (values t ts)))

;; Used to generalize return types
(define generalize-type-table
  (list
   ;; Don't distinguish -0.0 and 0.0
   (cons (set 'FlonumNegZero) (set 'FlonumNegZero 'FlonumPosZero))
   (cons (set 'FlonumPosZero) (set 'FlonumPosZero 'FlonumNegZero))
   (cons (set 'SingleFlonumNegZero) (set 'SingleFlonumNegZero 'SingleFlonumPosZero))
   (cons (set 'SingleFlonumPosZero) (set 'SingleFlonumPosZero 'SingleFlonumNegZero))
   ;; Don't distinguish non-integer rationals from rationals
   (cons (set 'PosRatNotInt) (set 'PosRatNotInt 'PosInt))
   (cons (set 'NegRatNotInt) (set 'NegRatNotInt 'NegInt))))

;; Returns a generalization of the return type `orig-t' by substituting from `generalize-type-table'
(define (generalize-return-type orig-t)
  (let loop ([t orig-t] [table generalize-type-table])
    (cond [(empty? table)  (if (equal? t orig-t) t (generalize-return-type t))]
          [else  (define fst (first table))
                 (cond [(subset? (car fst) t)
                        (loop (type-union (set-subtract t (car fst)) (cdr fst))
                              (rest table))]
                       [else
                        (loop t (rest table))])])))

;; Generalizes all the return types in a set of function types
(define (generalize-return-types ts)
  (for/set ([t  (in-set ts)])
    (match-define (fun-type arg res) t)
    (fun-type arg (generalize-return-type res))))

;; Joins all function types with the same return type
(define (compress-fun-types ts)
  (define h (make-hash))
  (for/set ([t  (in-set ts)])
    (define tz (fun-type-res t))
    (define tx (fun-type-arg t))
    (hash-set! h tz (type-union tx (hash-ref h tz (λ () (set))))))
  (for/set ([(tz tx)  (in-hash h)])
    (fun-type tx tz)))

;; Turns sets of singleton products into sets of larger products
(define (compress-product-type ts)
  (define hx (make-hash))
  (for/set ([t  (in-set ts)])
    (match-define (list tx ty) t)
    (hash-set! hx ty (type-union tx (hash-ref hx ty (λ () (set))))))
  (define hy (make-hash))
  (for/set ([(ty txs)  (in-hash hx)])
    (hash-set! hy txs (type-union ty (hash-ref hy txs (λ () (set))))))
  (for/set ([(txs tys)  (in-hash hy)])
    (list txs tys)))

;; Compresses product types in a set of (binary) function types
(define (compress-arg-product-types ts)
  (for/set ([t  (in-set ts)])
    (match-define (fun-type arg res) t)
    (fun-type (compress-product-type arg) res)))

;; Simplifies a set of symbols by substituting from `simplify-type-table'
(define (simplify-flat-type orig-t)
  (let loop ([t orig-t] [table simplify-type-table])
    (cond [(empty? table)  (if (equal? t orig-t) t (simplify-flat-type t))]
          [else  (define fst (first table))
                 (cond [(subset? (car fst) t)
                        (loop (type-union (set-subtract t (car fst)) (cdr fst))
                              (rest table))]
                       [else
                        (loop t (rest table))])])))

;; Simplifies any type by recurring into it and simplifying sets of symbols
(define (simplify-type t)
  (cond [(set? t)
         (cond [(andmap values (set-map t symbol?))
                (simplify-flat-type t)]
               [else
                (for/set ([t  (in-set t)]) (simplify-type t))])]
        [(symbol? t)  t]
        [(fun-type? t)  (match-define (fun-type arg res) t)
                        (fun-type (simplify-type arg) (simplify-type res))]
        [(list? t)  (map simplify-type t)]
        [else  (error 'simplify-type)]))

(define (unsimplify-flat-type-once t)
  (hash-ref unsimplify-type-hash t (λ () (set t))))

;; Expands a symbol or a set of symbols into its full union form
(define (unsimplify-flat-type t)
  (let loop ([ts  (if (set? t) t (set t))])
    (define new-ts (apply set-union (set-map ts unsimplify-flat-type-once)))
    (cond [(equal? new-ts ts)  ts]
          [else  (loop new-ts)])))

;; Runs every simplification on a set of unary function types
(define (simplify-unary-types ts)
  (let loop ([ts ts])
    (define new-ts (compress-fun-types (generalize-return-types ts)))
    (cond [(equal? ts new-ts)  (simplify-type ts)]
          [else  (loop new-ts)])))

;; Runs every simplification on a set of binary function types
(define (simplify-binary-types ts)
  (let loop ([ts ts])
    (define new-ts (compress-fun-types (generalize-return-types ts)))
    (cond [(equal? ts new-ts)  (simplify-type (compress-arg-product-types ts))]
          [else  (loop new-ts)])))

;; ===================================================================================================
;; Test arguments

(define +max.f (real->single-flonum (- (expt 2 128) (expt 2 (- 128 24)))))
(define +min.f (real->single-flonum (expt 2 -127)))
(define -max.f (- +max.f))
(define -min.f (- +min.f))

;; Integers

(define positive-integers
  (list->set
   (append
    ;; Get some odd/even pairs at powers of 2 (some are perfect squares)
    (append* (for/list ([i  (in-range 1 10)])
               (list (expt 2 (- i 1))
                     (- (expt 2 i) 1))))
    ;; Some very large numbers
    (list (inexact->exact (sqrt +max.0))
          (inexact->exact +max.0)  ; perfect square
          (* 2 (inexact->exact +max.0))))))

(define negative-integers
  (list->set
   (append
    ;; Get some odd/even pairs at negative powers of 2
    (append* (for/list ([i  (in-range 1 10)])
               (list (- (expt 2 i))
                     (- (- (expt 2 i)) 1))))
    ;; Some very large numbers
    (list (- (inexact->exact (sqrt +max.0)))
          (- (inexact->exact +max.0))
          (- (* 2 (inexact->exact +max.0)))))))

(define integers (set-union (set 0) positive-integers negative-integers))

;; Small integers

(define small-positive-integers (set 1 2 4 10))
(define small-negative-integers (set -1 -2 -4 -10))
(define small-integers (set-union (set 0) small-positive-integers small-negative-integers))

;; Rationals

(define positive-rationals
  (list->set
   (append
    (for*/list ([n  (in-set small-positive-integers)]
                [d  (in-set small-positive-integers)]
                [x  (in-value (/ n d))]
                #:when (not (integer? x)))
      x)
    ;; Some very small numbers
    (list (* 1/2 (inexact->exact +min.0))
          (inexact->exact +min.0)
          (inexact->exact (sqrt +min.0))))))

(define negative-rationals
  (list->set (set-map positive-rationals -)))

(define rationals (set-union positive-rationals negative-rationals))

;; Flonums

(define flonums
  (set-union
   (list->set (set-map (set-union rationals small-integers) real->double-flonum))
   (set -inf.0 -max.0 (- (sqrt +max.0)) (- (sqrt +min.0)) -min.0 -0.0
        +inf.0 +max.0    (sqrt +max.0)     (sqrt +min.0)  +min.0 +0.0 +nan.0)))

(define single-flonums
  (set-union
   (list->set (set-map (set-union rationals small-integers) real->single-flonum))
   (set -inf.f -max.f (- (sqrt +max.f)) (- (sqrt +min.f)) -min.f -0.0f0
        +inf.f +max.f    (sqrt +max.f)     (sqrt +min.f)  +min.f +0.0f0 +nan.f)))

;; Collect and type argument values

(define argument-values (make-hasheq (list (cons 'Nothing (set)))))

(define (add-value! x)
  (define t (value-type x))
  (hash-set! argument-values t (set-add (hash-ref argument-values t (λ () (set))) x)))

(for ([x  (in-set integers)]) (add-value! x))
(for ([x  (in-set rationals)]) (add-value! x))
(for ([x  (in-set flonums)]) (add-value! x))
(for ([x  (in-set single-flonums)]) (add-value! x))
(for* ([x  (in-set (set-union small-integers rationals flonums single-flonums))]
       [y  (in-set (set-union small-integers rationals flonums single-flonums))])
  (add-value! (make-rectangular x y)))

;; Make sure every numeric type is a key in `argument-values'
(for ([t  (in-set types)])
  (hash-ref argument-values t))

;; Make sure every key in `argument-values' is a numeric type
(for ([t  (in-hash-keys argument-values)])
  (unless (set-member? types t)
    (error 'bad)))

(let ([n  (apply + (hash-map argument-values (λ (k v) (set-count v))))])
  (printf "~a total test arguments (~a pairs for binary functions)~n~n" n (sqr n)))

;; ===================================================================================================
;; Inferring types from example values

;; Applies `f' to `xs' and returns the type of the value
;; If `f' raises a "skip" exception, the type is 'skip
;; If it raises any other exception, the type is 'Nothing
(define (result-type f . xs)
  (with-handlers ([exn?  (λ (e) (cond [(equal? (exn-message e) "skip")  'skip]
                                      [else  'Nothing]))])
    (value-type (apply f xs))))

;; Infers the type of `f' for the single argument type `tx'; ignores 'skip types
(define (unary-types f tx)
  (fun-type (set tx)
            (for*/set ([x  (in-set (hash-ref argument-values tx))]
                       [t  (in-value (result-type f x))]
                       #:unless (equal? t 'skip))
              t)))

;; Infers the type of `f' for the argument types `tx' and `ty'; ignores 'skip types
(define (binary-types f tx ty)
  (fun-type (set (list tx ty))
            (for*/set ([x  (in-set (hash-ref argument-values tx))]
                       [y  (in-set (hash-ref argument-values ty))]
                       [t  (in-value (result-type f x y))]
                       #:unless (equal? t 'skip))
              t)))

;; Infers all the types of `f' from representative argument values
(define (infer-unary-types f)
  (for/set ([tx  (in-list type-list)]
            #:unless (equal? tx 'Nothing))
    (printf "~a: ~a~n" f tx)
    (flush-output)
    (unary-types f tx)))

;; Infers all the types of `f' from representative argument values
(define (infer-binary-types f)
  (for*/set ([tx  (in-list type-list)]
             #:unless (equal? tx 'Nothing)
             [ty  (in-list type-list)]
             #:unless (equal? ty 'Nothing))
    (printf "~a: ~a ~a~n" f tx ty)
    (flush-output)
    (binary-types f tx ty)))

;; ===================================================================================================
;; Getting examples of argument and result types

(define (argument-type-examples t)
  (define ts (unsimplify-flat-type t))
  (apply set-union (set-map ts (λ (t) (hash-ref argument-values t)))))

(define (unary-type-examples f tx ty)
  (define xs (argument-type-examples tx))
  (define tys (unsimplify-flat-type ty))
  (for*/set ([x  (in-set xs)]
             [t  (in-value (result-type f x))]
             #:when (set-member? tys t))
    (list x (with-handlers ([exn? (λ (e) e)])
              (f x)))))

(define (binary-type-examples f tx ty tz)
  (define xs (argument-type-examples tx))
  (define ys (argument-type-examples ty))
  (define tzs (unsimplify-flat-type tz))
  (for*/set ([x  (in-set xs)]
             [y  (in-set ys)]
             [t  (in-value (result-type f x y))]
             #:when (set-member? tzs t))
    (list x y (with-handlers ([exn? (λ (e) e)])
                (f x y)))))

;; ===================================================================================================
;; Wrappers

;; Exponential: a lot of rational+integer argument combos will eat up all the memory, so create a
;; wrapper `wussy-expt' that raises a "skip" exception when that would happen

(define (expt-digits x y)
  (let ([x  (abs x)])
    (cond [(or (zero? x) (not (rational? x)))  0]
          [else
           (/ (+ (* (magnitude y) (log (numerator x)))
                 (* (magnitude y) (log (denominator x))))
              (log 10))])))

(define (wussy-expt x y)
  (cond [(or (not (exact? x)) (not (exact? y)))
         (expt x y)]
        [(< (max (+ (expt-digits (real-part x) y)
                    (expt-digits (imag-part x) y))
                 (expt-digits (magnitude x) y))
            10000)
         (expt x y)]
        [else         
         (error "skip")]))

;; ===================================================================================================
;; Finally, actual use

(define sqrt-types
  (simplify-unary-types (infer-unary-types sqrt)))

#;; This takes a few minutes
(define expt-types
  (simplify-binary-types (infer-binary-types wussy-expt)))

(printf "~nInferred types for sqrt:~n")
sqrt-types
(printf "~nHas type SingleFloatComplex -> (U FloatComplex SingleFloatComplex); what's going on?~n")
(unary-type-examples sqrt 'SingleFloatComplex 'FloatComplex)
