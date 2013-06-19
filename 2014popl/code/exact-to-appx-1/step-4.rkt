#lang typed/racket

;; Changes from step-3.rkt:
;;  * Changed type of preimages to use lazy mappings with standard algebras

;; Proof obligations:
;;  * Forgetting measurable sets doesn't change computed preimages for measurable sets

(require "../set-ops.rkt"
         "../standard-algebra.rkt"
         "preimage.rkt"
         "mapping.rkt"
         "map-arrow.rkt"
         typed/rackunit)

;; ===================================================================================================
;; Preimage arrow

(define-type (Pre-Arrow X Y) ((Setof X) -> (std-preimage X Y)))

(: pre-arr (All (X Y) ((X -> Y) -> (Pre-Arrow X Y))))
(define ((pre-arr f) A)
  (std-preimage (λ: ([B : (Setof Y)]) (set-preimage f A B))
                (set-image f A)))

(: pre->>> (All (X Y Z) ((Pre-Arrow X Y) (Pre-Arrow Y Z) -> (Pre-Arrow X Z))))
(define ((pre->>> f1 f2) A)
  (let ([f1  (f1 A)])
    (let ([f2  (f2 (std-preimage-range f1))])
      (std-preimage-compose f2 f1))))

(: pre-pair (All (X Y Z) ((Pre-Arrow X Y) (Pre-Arrow X Z) -> (Pre-Arrow X (Pair Y Z)))))
(define ((pre-pair f1 f2) A)
  (let ([f1  (f1 A)]
        [f2  (f2 A)])
    ;; Doing this obvious thing in the function:
    ;;
    ;;   (set-intersect (std-preimage-ap f1 (set-fst BC))
    ;;                  (std-preimage-ap f2 (set-snd BC)))
    ;;
    ;; makes `pre-pair' compute preimages of *approximate* output sets, which makes the preimages
    ;; themselves approximate. On the other hand, the fact that flattening the algebra causes
    ;; computations to become approximate could be seen as "elegant"...
    (std-preimage (λ: ([BC : (Setof (Pair Y Z))])
                    (set-bind BC (λ: ([bc : (Pair Y Z)])
                                   (set-intersect ((std-preimage-function f1) (set (car bc)))
                                                  ((std-preimage-function f2) (set (cdr bc)))))))
                  (set-product (std-preimage-range f1)
                               (std-preimage-range f2)))))

(: pre-if (All (X Y) ((Pre-Arrow X Boolean) (Pre-Arrow X Y) (Pre-Arrow X Y) -> (Pre-Arrow X Y))))
(define ((pre-if c t f) A)
  (let ([c  (c A)])
    (let ([t  (t (std-preimage-ap c (set #t)))]
          [f  (f (std-preimage-ap c (set #f)))])
      (std-preimage (λ: ([B : (Setof Y)])
                      (set-union (std-preimage-ap t B)
                                 (std-preimage-ap f B)))
                    (set-union (std-preimage-range t)
                               (std-preimage-range f))))))

(: pre-id (All (X) (Pre-Arrow X X)))
(define (pre-id A)
  (((inst pre-arr X X) (λ (x) x)) A))

(: pre-const (All (X Y) (Y -> (Pre-Arrow X Y))))
(define ((pre-const y) A)
  (((inst pre-arr X Y) (λ (x) y)) A))

(: pre-fst (All (X Y) (Pre-Arrow (Pair X Y) X)))
(define (pre-fst A)
  (((inst pre-arr (Pair X Y) X) car) A))

(: pre-snd (All (X Y) (Pre-Arrow (Pair X Y) Y)))
(define (pre-snd A)
  (((inst pre-arr (Pair X Y) Y) cdr) A))

;; ===================================================================================================
;; Tests

(: test-comps (All (X Y) ((Map-Arrow X Y) (Pre-Arrow X Y) (Setof X) (Setof Y) Any
                                          -> Void)))
(define (test-comps map-comp pre-comp A B print?)
  (for: ([A  (in-list (set->list (set-power A)))])
    (define pre (std-preimage-close (pre-comp A)))
    (define Bs (std-preimage-range-algebra pre))
    (when print?
      (printf "A = ~v~n" A)
      (printf "Bs = ~v~n" Bs))
    (check-true (equal? Bs (standard-algebra (std-preimage-range pre))))
    (for: ([B  (in-list (set->list (set-power B)))])
      (cond [(set-member? Bs B)
             (when print? (printf "B = ~v~n" B))
             (define Am (mapping-preimage (map-comp A) B))
             (define Ap (std-preimage-ap pre B))
             (check-true (equal? Am Ap))
             (when print? (printf "~n"))]
            [else
             (check-exn exn:fail:contract? (λ () (std-preimage-ap pre B)))]))))

(: A (Setof (Pair Boolean Symbol)))
(define A (set (cons #t 'a) (cons #t 'b) (cons #t 'c)
               (cons #f 'b) (cons #f 'c) (cons #f 'd)))

(printf "********** Test: const **********~n")
(test-comps ((inst map-const (Pair Boolean Symbol) Boolean) #t)
            ((inst pre-const (Pair Boolean Symbol) Boolean) #t)
            A
            (set #t #f)
            #f)

(printf "********** Test: fst **********~n")
(test-comps (inst map-fst Boolean Symbol)
            (inst pre-fst Boolean Symbol)
            A
            (set #t #f)
            #f)

(printf "********** Test: pair, const **********~n")
(test-comps (map-pair ((inst map-const (Pair Boolean Symbol) Boolean) #t)
                      ((inst map-const (Pair Boolean Symbol) Boolean) #f))
            (pre-pair ((inst pre-const (Pair Boolean Symbol) Boolean) #t)
                      ((inst pre-const (Pair Boolean Symbol) Boolean) #f))
            A
            (set-product (set #t #f) (set #t #f))
            #f)

(printf "********** Test: pair, fst, snd **********~n")
(test-comps (map-pair (inst map-fst Boolean Symbol)
                      (inst map-snd Boolean Symbol))
            (pre-pair (inst pre-fst Boolean Symbol)
                      (inst pre-snd Boolean Symbol))
            A
            (set-product (set #t #f) (set 'a 'b 'c))
            #f)

(printf "********** Test: if, fst, snd, const **********~n")
(test-comps (map-if (inst map-fst Boolean Symbol)
                    (inst map-snd Boolean Symbol)
                    ((inst map-const (Pair Boolean Symbol) Symbol) 'bad))
            (pre-if (inst pre-fst Boolean Symbol)
                    (inst pre-snd Boolean Symbol)
                    ((inst pre-const (Pair Boolean Symbol) Symbol) 'bad))
            A
            (set 'a 'b 'c 'd 'bad)
            #f)
