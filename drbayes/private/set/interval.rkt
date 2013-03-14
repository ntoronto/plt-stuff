#lang racket/base

(provide Interval Maybe-Interval
         interval interval? interval-min interval-max interval-min? interval-max?
         real-interval positive-interval negative-interval
         zero-interval nonpositive-interval nonnegative-interval
         unit-interval
         interval-intersect
         interval-join
         interval-subseteq?
         interval-member?
         interval-disjoint?
         interval-sample-point
         interval-measure)

(module typed-defs typed/racket/base
  (provide (all-defined-out))
  
  (require racket/match
           racket/flonum
           racket/list
           math/private/utils
           "extremal-set.rkt")
  
  (: print-interval (Interval Output-Port (U #t #f 0 1) -> Any))
  (define (print-interval I port mode)
    (match-define (Interval a b a? b?) I)
    (cond [(and a? b?)  (pretty-print-constructor 'interval (list a b) port mode)]
          [else  (pretty-print-constructor 'interval (list a b a? b?) port mode)]))
  
  (struct: Interval ([min : Flonum] [max : Flonum] [min? : Boolean] [max? : Boolean])
    #:transparent
    #:property prop:custom-print-quotable 'never
    #:property prop:custom-write print-interval)
  
  (define-type Maybe-Interval (U Empty-Set Interval))
  
  (define real-interval (Interval -inf.0 +inf.0 #f #f))
  (define positive-interval (Interval 0.0 +inf.0 #f #f))
  (define negative-interval (Interval -inf.0 -0.0 #f #f))
  (define nonnegative-interval (Interval 0.0 +inf.0 #t #f))
  (define nonpositive-interval (Interval -inf.0 -0.0 #f #t))
  (define zero-interval (Interval -0.0 +0.0 #t #t))
  (define unit-interval (Interval 0.0 1.0 #t #t))
  
  (: make-interval (case-> (Flonum Flonum -> Maybe-Interval)
                           (Flonum Flonum Boolean Boolean -> Maybe-Interval)))
  (define (make-interval a b [a? #t] [b? #t])
    (cond [(not (<= -inf.0 a +inf.0))  (raise-argument-error 'interval "Flonum, not NaN" 0 a b)]
          [(not (<= -inf.0 b +inf.0))  (raise-argument-error 'interval "Flonum, not NaN" 1 a b)]
          [(or (> a b) (and (= a b) (not (and a? b?))))  empty-set]
          [else
           (let ([a  (if (= a -0.0) +0.0 a)]
                 [b  (if (= b +0.0) -0.0 b)])
             (cond [(fl= a -inf.0)
                    (cond [(fl= b +inf.0)  real-interval]
                          [(fl= b -inf.0)  empty-set]
                          [else  (Interval -inf.0 b #f b?)])]
                   [(fl= b +inf.0)
                    (cond [(fl= a +inf.0)  empty-set]
                          ;[(fl= a -inf.0)  real-interval]  ; already checked above
                          [else  (Interval a +inf.0 a? #f)])]
                   [else  (Interval a b a? b?)]))]))
  
  (: interval-intersect (Interval Interval -> Maybe-Interval))
  (define (interval-intersect I1 I2)
    (match-define (Interval a1 b1 a1? b1?) I1)
    (match-define (Interval a2 b2 a2? b2?) I2)
    (define-values (a a?)
      (cond [(a1 . > . a2)  (values a1 a1?)]
            [(a1 . < . a2)  (values a2 a2?)]
            [else           (values a1 (and a1? a2?))]))
    (define-values (b b?)
      (cond [(b1 . > . b2)  (values b2 b2?)]
            [(b1 . < . b2)  (values b1 b1?)]
            [else           (values b1 (and b1? b2?))]))
    (cond [(and (fl= a a1) (fl= b b1) (eq? a? a1?) (eq? b? b1?))  I1]
          [(and (fl= a a2) (fl= b b2) (eq? a? a2?) (eq? b? b2?))  I2]
          [else  (make-interval a b a? b?)]))
  
  (: interval-join (Interval Interval -> Interval))
  (define (interval-join I1 I2)
    (match-define (Interval a1 b1 a1? b1?) I1)
    (match-define (Interval a2 b2 a2? b2?) I2)
    (define-values (a a?)
      (cond [(a1 . < . a2)  (values a1 a1?)]
            [(a1 . > . a2)  (values a2 a2?)]
            [else           (values a1 (or a1? a2?))]))
    (define-values (b b?)
      (cond [(b1 . > . b2)  (values b1 b1?)]
            [(b1 . < . b2)  (values b2 b2?)]
            [else           (values b1 (or b1? b2?))]))
    (cond [(and (fl= a a1) (fl= b b1) (eq? a? a1?) (eq? b? b1?))  I1]
          [(and (fl= a a2) (fl= b b2) (eq? a? a2?) (eq? b? b2?))  I2]
          [else  (assert (make-interval a b a? b?) Interval?)]))
  
  (: interval-subseteq? (Interval Interval -> Boolean))
  (define (interval-subseteq? I1 I2)
    (match-define (Interval a1 b1 a1? b1?) I1)
    (match-define (Interval a2 b2 a2? b2?) I2)
    (and (or (a1 . > . a2) (and (= a1 a2) (or (not a1?) a2?)))
         (or (b1 . < . b2) (and (= b1 b2) (or (not b1?) b2?)))))
  
  (: interval-member? (Interval Flonum -> Boolean))
  (define (interval-member? I x)
    (match-define (Interval a b a? b?) I)
    (cond [(< a x b)  #t]
          [(< x a)  #f]
          [(< b x)  #f]
          [(and (= x a) a?)  #t]
          [(and (= x b) b?)  #t]
          [else  #f]))
  
  (: interval-disjoint2? (Interval Interval -> Boolean))
  (define (interval-disjoint2? A B)
    (empty-set? (interval-intersect A B)))
  
  (: interval-disjoint? (Interval * -> Boolean))
  (define (interval-disjoint? . As)
    (let loop0 ([As As])
      (if (empty? As)
          #t
          (let-values ([(A As)  (values (first As) (rest As))])
            (let loop1 ([Bs As])
              (if (empty? Bs)
                  (loop0 As)
                  (and (interval-disjoint2? A (first Bs)) (loop1 (rest Bs)))))))))
  
  (: interval-sample-point (Interval -> Flonum))
  (define (interval-sample-point I)
    (match-define (Interval a b a? b?) I)
    (define m (- b a))
    (define x (+ a (* m (random))))
    (cond [(and (or (not (= x a)) a?) (or (not (= x b)) b?))  x]
          [(and a? b?)  (* 0.5 (+ a b))]
          [a?  a]
          [b?  b]
          [else  (* 0.5 (+ a b))]))
  
  (: interval-measure (Interval -> Flonum))
  (define (interval-measure I)
    (- (Interval-max I) (Interval-min I)))
  )

(module untyped-defs racket/base
  (provide (all-defined-out))
  
  (require (for-syntax racket/base)
           racket/match
           (submod ".." typed-defs)
           "../untyped-utils.rkt")
  
  (define-syntax interval? (make-rename-transformer #'Interval?))
  (define-syntax interval-min (make-rename-transformer #'Interval-min))
  (define-syntax interval-max (make-rename-transformer #'Interval-max))
  (define-syntax interval-min? (make-rename-transformer #'Interval-min?))
  (define-syntax interval-max? (make-rename-transformer #'Interval-max?))
  
  (define-match-expander interval
    (λ (stx)
      (syntax-case stx ()
        [(_ a b a? b?)  (syntax/loc stx (Interval a b a? b?))]))
    (make-head-form #'make-interval))
  
  )  ; module

(require (submod "." typed-defs)
         (submod "." untyped-defs))
