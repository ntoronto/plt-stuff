#lang typed/racket

;; Original definition of the exact preimage arrow

;; Proof obligations:
;;  * This is an arrow
;;  * f' = mapping f A, if f' and f are constructed using corresponding combinators from the mapping
;;    and function arrows, respectively
;;  * preimage-ap (f' A) B = mapping-preimage (f A) B, if f' and f are constructed using corresponding
;;    combinators from the preimage and mapping arrows, respectively

(require "../set-ops.rkt"
         "../standard-algebra.rkt"
         "types.rkt"
         "mapping.rkt"
         "preimage.rkt"
         ;"fun-arrow.rkt"
         "map-arrow.rkt"
         typed/rackunit)

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage arrow

;; A preimage arrow computation is a function from a domain to a mapping from measurable output sets
;; to measurable input sets (where the input sets are restricted to the given domain)
(define-type (Pre-Arrow X Y) ((Setof X) -> (Preimage X Y)))

(: pre (All (X Y) ((Mapping X Y) (Setof (Setof Y)) -> (Preimage X Y))))
(define (pre f Bs)
  (mapping (λ: ([B : (Setof Y)]) (mapping-preimage f B)) Bs))

(: pre-arr (All (X Y) ((Map-Arrow X Y) -> (Pre-Arrow X Y))))
(define ((pre-arr f) A)
  (let ([f  (f A)])
    (pre f (standard-algebra (mapping-range f)))))

(: pre->>> (All (X Y Z) ((Pre-Arrow X Y) (Pre-Arrow Y Z) -> (Pre-Arrow X Z))))
;; Composes two preimage arrow computations
(define ((pre->>> f1 f2) A)
  (let* ([f1  (f1 A)]
         [f2  (f2 (preimage-range f1))])
    (mapping (λ: ([C : (Setof (U Z Bottom))])
               (preimage-ap f1 (preimage-ap f2 C)))
             (mapping-domain f2))))

(: pre-pair (All (X Y Z) ((Pre-Arrow X Y) (Pre-Arrow X Z) -> (Pre-Arrow X (Pair Y Z)))))
;; Pairs two preimage arrow computations
(define ((pre-pair f1 f2) A)
  (let* ([f1  (f1 A)]
         [f2  (f2 A)])
    ;; Compute a mapping for just rectangles, then close it under set algebra operations
    (preimage-close
     (mapping (λ: ([BC : (Setof (Pair Y Z))])
                (set-intersect (preimage-ap f1 (set-fst BC))
                               (preimage-ap f2 (set-snd BC))))
              (set-algebra-rectangles (preimage-range-algebra f1)
                                      (preimage-range-algebra f2))))))

(: pre-if (All (X Y) ((Pre-Arrow X Boolean) (Pre-Arrow X Y) (Pre-Arrow X Y) -> (Pre-Arrow X Y))))
;; Strict conditional for preimage arrow computations
;; Note that this requires {true} and {false} to be measurable sets in the standard algebra
(define ((pre-if c t f) A)
  (let* ([c  (c A)]
         [t  (pre-run t (preimage-ap c (set #t)))]
         [f  (pre-run f (preimage-ap c (set #f)))])
    ;; Compute a mapping for a union algebra, then close it under set algebra operations
    (preimage-close
     (mapping (λ: ([B : (Setof Y)])
                (set-union (preimage-ap t B)
                           (preimage-ap f B)))
              (set-union (preimage-range-algebra t)
                         (preimage-range-algebra f))))))

(: pre-run (All (X Y) ((Pre-Arrow X Y) (Setof X) -> (Preimage X Y))))
(define (pre-run f A)
  (if (set-empty? A) 
      (pre-bottom A)
      #;; Same thing:
      (set (cons ((inst set Y)) ((inst set X))))
      (f A)))

(: pre-lazy (All (X Y) ((-> (Pre-Arrow X Y)) -> (Pre-Arrow X Y))))
(define ((pre-lazy f) A)
  ((f) A))

;; Some useful lifts

(: pre-id (All (X) (Pre-Arrow X X)))
(define (pre-id A)
  (((inst pre-arr X X) map-id) A))

(: pre-const (All (X Y) (Y -> (Pre-Arrow X Y))))
(define ((pre-const y) A)
  (((inst pre-arr X Y) (map-const y)) A))

(: pre-fst (All (X Y) (Pre-Arrow (Pair X Y) X)))
(define (pre-fst A)
  (((inst pre-arr (Pair X Y) X) map-fst) A))

(: pre-snd (All (X Y) (Pre-Arrow (Pair X Y) Y)))
(define (pre-snd A)
  (((inst pre-arr (Pair X Y) Y) map-snd) A))

(: pre-bottom (All (X) (Pre-Arrow X Nothing)))
(define (pre-bottom A)
  (((inst pre-arr X Nothing) map-bottom) A))

#|
;; ===================================================================================================
;; Tests against the function and mapping arrows

;; These test the second proof obligation above (i.e. that the preimage arrow computes preimages), by
;; constructing preimage arrow computations, and testing them against corresponding function and
;; mapping arrow computations

(: test-comps (All (X Y) ((Fun-Arrow X Y) (Map-Arrow X Y) (Pre-Arrow X Y) (Setof X) (Setof Y) Any
                                          -> Void)))
(define (test-comps fun-comp map-comp pre-comp A B print?)
  ;; For each subset of A
  (for: ([A  (in-list (set->list (set-power A)))])
    ;; Make a preimage arrow computation, get its output algebra
    (define pre (pre-comp A))
    (define Bs (preimage-range-algebra pre))
    (when print?
      (printf "A = ~v~n" A)
      (printf "Bs = ~v~n" Bs))
    ;; Make sure its range algebra is the standard one
    (check-true (equal? Bs (standard-algebra (preimage-range pre))))
    ;; For each subset of B
    (for: ([B  (in-list (set->list (set-power B)))])
      (cond [(set-member? Bs B)
             ;; If B is in the range algebra, test the preimage computation's preimages against the
             ;; preimages computed using the function and mapping arrows
             (when print? (printf "B = ~v~n" B))
             (define Af (set-preimage fun-comp A B))
             (define Am (mapping-preimage (map-comp A) B))
             (define Ap (preimage-ap pre B))
             (check-true (equal? Af Am))
             (check-true (equal? Af Ap))
             (when print? (printf "~n"))]
            [else
             ;; If not, ensure that trying to compute a preimage raises an error
             (check-exn exn:fail:contract? (λ () (preimage-ap pre B)))]))))

(: A (Setof (Pair Boolean Symbol)))
(define A (set (cons #t 'a) (cons #t 'b) (cons #t 'c)
               (cons #f 'b) (cons #f 'c) (cons #f 'd)))

(printf "********** Test: const **********~n")
(test-comps ((inst fun-const (Pair Boolean Symbol) Boolean) #t)
            ((inst map-const (Pair Boolean Symbol) Boolean) #t)
            ((inst pre-const (Pair Boolean Symbol) Boolean) #t)
            A
            (set #t #f)
            #f)

(printf "********** Test: fst **********~n")
(test-comps (inst fun-fst Boolean Symbol)
            (inst map-fst Boolean Symbol)
            (inst pre-fst Boolean Symbol)
            A
            (set #t #f)
            #f)

(printf "********** Test: pair, const **********~n")
(test-comps (fun-pair ((inst fun-const (Pair Boolean Symbol) Boolean) #t)
                      ((inst fun-const (Pair Boolean Symbol) Boolean) #f))
            (map-pair ((inst map-const (Pair Boolean Symbol) Boolean) #t)
                      ((inst map-const (Pair Boolean Symbol) Boolean) #f))
            (pre-pair ((inst pre-const (Pair Boolean Symbol) Boolean) #t)
                      ((inst pre-const (Pair Boolean Symbol) Boolean) #f))
            A
            (set-product (set #t #f) (set #t #f))
            #f)

(printf "********** Test: pair, fst, snd **********~n")
(test-comps (fun-pair (inst fun-fst Boolean Symbol)
                      (inst fun-snd Boolean Symbol))
            (map-pair (inst map-fst Boolean Symbol)
                      (inst map-snd Boolean Symbol))
            (pre-pair (inst pre-fst Boolean Symbol)
                      (inst pre-snd Boolean Symbol))
            A
            (set-product (set #t #f) (set 'a 'b 'c))
            #f)

(printf "********** Test: if, fst, snd, const **********~n")
(test-comps (fun-if (inst fun-fst Boolean Symbol)
                    (inst fun-snd Boolean Symbol)
                    ((inst fun-const (Pair Boolean Symbol) Symbol) 'bad))
            (map-if (inst map-fst Boolean Symbol)
                    (inst map-snd Boolean Symbol)
                    ((inst map-const (Pair Boolean Symbol) Symbol) 'bad))
            (pre-if (inst pre-fst Boolean Symbol)
                    (inst pre-snd Boolean Symbol)
                    ((inst pre-const (Pair Boolean Symbol) Symbol) 'bad))
            A
            (set 'a 'b 'c 'd 'bad)
            #f)
|#


;(define pre-halt-on-true (pre-arr map-halt-on-true))

(: pre-halt-on-true (Pre-Arrow Boolean Boolean))
(define pre-halt-on-true
  (pre-if (inst pre-id Boolean)
          (pre-lazy (λ () ((inst pre-const Boolean Boolean) #t)))
          (pre-lazy (λ () pre-halt-on-true))))

(pre-halt-on-true (set #t))
;; Diverges:
;(pre-halt-on-true (set #f))
