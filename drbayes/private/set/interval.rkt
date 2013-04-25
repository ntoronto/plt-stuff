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
         interval-sample-point
         interval-measure
         Interval-List Interval* Maybe-Interval*
         interval-list
         interval-list?
         interval-list-elements
         interval*-subtract
         interval*-union
         interval*-intersect
         interval*-subseteq?
         interval*-member?
         interval*-sample-point
         interval*-measure
         interval*-map/mono
         interval*-map
         )

(module typed-defs typed/racket/base
  (provide (all-defined-out))
  
  (require racket/match
           racket/flonum
           racket/list
           math/private/utils
           math/flonum
           math/distributions
           "extremal-set.rkt"
           "../utils.rkt")
  
  (define check-interval*-valid? #t)
  
  (: print-interval (Interval Output-Port (U #t #f 0 1) -> Any))
  (define (print-interval I port mode)
    (match-define (Interval a b a? b?) I)
    (cond [(and a? b?)  (pretty-print-constructor 'interval (list a b) port mode)]
          [else  (pretty-print-constructor 'interval (list a b a? b?) port mode)]))
  
  (: interval-guard (Flonum Flonum Boolean Boolean Symbol -> (Values Flonum Flonum Boolean Boolean)))
  (define (interval-guard a b a? b? name)
    (cond [(and (or (a . < . b) (and (= a b) a? b?))
                (if (= a -inf.0) (not a?) #t)
                (if (= b +inf.0) (not b?) #t))
           (values a b a? b?)]
          [else
           (error name "expected strictly increasing endpoints; given ~a ~a ~a ~a" a b a? b?)]))
  
  (struct: Interval ([min : Flonum] [max : Flonum] [min? : Boolean] [max? : Boolean])
    #:transparent
    #:guard interval-guard
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
  
  ;; =================================================================================================
  ;; Sorted, disjoint interval unions
  
  (: interval-list-valid? ((Listof+2 Interval) -> Boolean))
  (define (interval-list-valid? Is)
    (let loop ([I1  (first Is)] [I2  (second Is)] [Is  (rest (rest Is))])
      (match-define (Interval a1 b1 a1? b1?) I1)
      (match-define (Interval a2 b2 a2? b2?) I2)
      (cond [(not (or (b1 . < . a2) (and (= b1 a2) (not b1?) (not a2?))))  #f]
            [(empty? Is)  #t]
            [else  (loop I2 (first Is) (rest Is))])))
  
  (: interval*-guard ((Listof+2 Interval) Symbol -> (Listof+2 Interval)))
  (define (interval*-guard Is name)
    (cond [(interval-list-valid? Is)  Is]
          [else
           (error name "expected strictly increasing, nonoverlapping intervals; given ~e" Is)]))
  
  (struct: Interval-List ([elements : (Listof+2 Interval)])
    #:transparent
    #:guard interval*-guard)
  
  (define-type Interval* (U Interval Interval-List))
  (define-type Maybe-Interval* (U Empty-Set Interval*))
  
  (: interval-list (case-> ((Pair Interval (Listof Interval)) -> Interval*)
                           ((Listof Interval) -> Maybe-Interval*)))
  (define (interval-list Is)
    (cond [(empty? Is)  empty-set]
          [(empty? (rest Is))  (first Is)]
          [else  (Interval-List Is)]))
  
  (: min<? (Flonum Boolean Flonum Boolean -> Boolean))
  (define (min<? a1 a1? a2 a2?)
    (or (a1 . < . a2) (and (= a1 a2) a1? (not a2?))))
  
  (: max<? (Flonum Boolean Flonum Boolean -> Boolean))
  (define (max<? b1 b1? b2 b2?)
    (or (b1 . < . b2) (and (= b1 b2) (not b1?) b2?)))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Subtraction
  
  (: interval-list-subtract ((Listof Interval) (Listof Interval) -> (Listof Interval)))
  (define (interval-list-subtract I1 I2)
    (cond
      [(or (empty? I1) (empty? I2))  I1]
      [else
       (match-define (Interval a1 b1 a1? b1?) (first I1))
       (match-define (Interval a2 b2 a2? b2?) (first I2))
       (cond
         [(or (b1 . < . a2) (and (= b1 a2) (or (not b1?) (not a2?))))
          ;; ------
          ;;       ------
          (cons (first I1) (interval-list-subtract (rest I1) I2))]
         [(or (b2 . < . a1) (and (= b2 a1) (or (not b2?) (not a1?))))
          ;;       ------
          ;; ------
          (interval-list-subtract I1 (rest I2))]
         [(min<? a1 a1? a2 a2?)
          (cond [(max<? b2 b2? b1 b1?)
                 ;; ------
                 ;;   --
                 (define I3 (assert (make-interval a1 a2 a1? (not a2?)) Interval?))
                 (define I4 (assert (make-interval b2 b1 (not b2?) b1?) Interval?))
                 (cons I3 (interval-list-subtract (cons I4 (rest I1)) (rest I2)))]
                [else
                 ;; ------           ------
                 ;;    ------   or      ---
                 (define I3 (assert (make-interval a1 a2 a1? (not a2?)) Interval?))
                 (cons I3 (interval-list-subtract (rest I1) I2))])]
         [else
          (cond [(max<? b2 b2? b1 b1?)
                 ;;    ------        ------
                 ;; ------      or   ---
                 (define I4 (assert (make-interval b2 b1 (not b2?) b1?) Interval?))
                 (interval-list-subtract (cons I4 (rest I1)) (rest I2))]
                [else
                 ;;   --             ---        ---           ------
                 ;; ------   or   ------   or   ------   or   ------
                 (interval-list-subtract (rest I1) I2)])])]))
  
  (: interval*-subtract (Maybe-Interval* Maybe-Interval* -> Maybe-Interval*))
  (define (interval*-subtract I1 I2)
    (cond [(empty-set? I1)  empty-set]
          [(empty-set? I2)  I1]
          [else  (interval-list
                  (interval-list-subtract
                   (if (Interval? I1) (list I1) (Interval-List-elements I1))
                   (if (Interval? I2) (list I2) (Interval-List-elements I2))))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Union
  
  (: interval-list-union ((Listof Interval) (Listof Interval) -> (Listof Interval)))
  (define (interval-list-union I1 I2)
    (cond
      [(empty? I1)  I2]
      [(empty? I2)  I1]
      [else
       (match-define (Interval a1 b1 a1? b1?) (first I1))
       (match-define (Interval a2 b2 a2? b2?) (first I2))
       (cond
         [(or (b1 . < . a2) (and (= b1 a2) (not b1?) (not a2?)))
          ;; ------
          ;;        ------
          (cons (first I1) (interval-list-union (rest I1) I2))]
         [(or (b2 . < . a1) (and (= b2 a1) (not b2?) (not a1?)))
          ;;        ------
          ;; ------
          (cons (first I2) (interval-list-union I1 (rest I2)))]
         [(min<? a1 a1? a2 a2?)
          (cond [(max<? b2 b2? b1 b1?)
                 ;; ------
                 ;;   --
                 (interval-list-union I1 (rest I2))]
                [else
                 ;; ------           ------
                 ;;    ------   or      ---
                 (define I (assert (make-interval a1 b2 a1? b2?) Interval?))
                 (interval-list-union (rest I1) (cons I (rest I2)))])]
         [else
          (cond [(max<? b2 b2? b1 b1?)
                 ;;    ------        ------
                 ;; ------      or   ---
                 (define I (assert (make-interval a2 b1 a2? b1?) Interval?))
                 (interval-list-union (cons I (rest I1)) (rest I2))]
                [else
                 ;;   --             ---        ---           ------
                 ;; ------   or   ------   or   ------   or   ------
                 (interval-list-union (rest I1) I2)])])]))
  
  (: interval*-union (case-> (Interval* Interval* -> Interval*)
                             (Maybe-Interval* Maybe-Interval* -> Maybe-Interval*)))
  (define (interval*-union I1 I2)
    (cond [(empty-set? I1)  I2]
          [(empty-set? I2)  I1]
          [else
           (define Is
             (interval-list-union
              (if (Interval? I1) (list I1) (Interval-List-elements I1))
              (if (Interval? I2) (list I2) (Interval-List-elements I2))))
           (cond [(empty? Is)
                  (raise-result-error 'interval-list-union "nonempty (Listof Interval)" Is)]
                 [else
                  (interval-list Is)])]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Intersection
  
  (: interval-list-intersect ((Listof Interval) (Listof Interval) -> (Listof Interval)))
  (define (interval-list-intersect I1 I2)
    (cond
      [(empty? I1)  I1]
      [(empty? I2)  I2]
      [else
       (match-define (Interval a1 b1 a1? b1?) (first I1))
       (match-define (Interval a2 b2 a2? b2?) (first I2))
       (cond
         [(or (b1 . < . a2) (and (= b1 a2) (or (not b1?) (not a2?))))
          ;; ------
          ;;       ------
          (interval-list-intersect (rest I1) I2)]
         [(or (b2 . < . a1) (and (= b2 a1) (or (not b2?) (not a1?))))
          ;;       ------
          ;; ------
          (interval-list-intersect I1 (rest I2))]
         [(min<? a1 a1? a2 a2?)
          (cond [(max<? b2 b2? b1 b1?)
                 ;; ------
                 ;;   --
                 (cons (first I2) (interval-list-intersect I1 (rest I2)))]
                [else
                 ;; ------           ------
                 ;;    ------   or      ---
                 (define I (assert (make-interval a2 b1 a2? b1?) Interval?))
                 (cons I (interval-list-intersect (rest I1) I2))])]
         [else
          (cond [(max<? b2 b2? b1 b1?)
                 ;;    ------        ------
                 ;; ------      or   ---
                 (define I (assert (make-interval a1 b2 a1? b2?) Interval?))
                 (cons I (interval-list-intersect I1 (rest I2)))]
                [else
                 ;;   --             ---        ---           ------
                 ;; ------   or   ------   or   ------   or   ------
                 (cons (first I1) (interval-list-intersect (rest I1) I2))])])]))
  
  (: interval*-intersect (Maybe-Interval* Maybe-Interval* -> Maybe-Interval*))
  (define (interval*-intersect I1 I2)
    (cond [(empty-set? I1)  I1]
          [(empty-set? I2)  I2]
          [(and (Interval? I1) (Interval? I2))  (interval-intersect I1 I2)]
          [else  (interval-list
                  (interval-list-intersect
                   (if (Interval? I1) (list I1) (Interval-List-elements I1))
                   (if (Interval? I2) (list I2) (Interval-List-elements I2))))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Subset
  
  (: interval-list-subseteq? ((Listof Interval) (Listof Interval) -> Boolean))
  (define (interval-list-subseteq? I1 I2)
    (cond [(empty? I1)  #t]
          [(empty? I2)  #f]
          [else
           (match-define (Interval a1 b1 a1? b1?) (first I1))
           (match-define (Interval a2 b2 a2? b2?) (first I2))
           (cond
             [(or (b1 . < . a2) (and (= b1 a2) (or (not b1?) (not a2?))))
              ;; ------
              ;;       ------
              #f]
             [(or (b2 . < . a1) (and (= b2 a1) (or (not b2?) (not a1?))))
              ;;       ------
              ;; ------
              (interval-list-subseteq? I1 (rest I2))]
             [(min<? a1 a1? a2 a2?)
              ;; ------        ------           ------
              ;;   --     or      ------   or      ---
              #f]
             [(max<? b2 b2? b1 b1?)
              ;;    ------        ------
              ;; ------      or   ---
              #f]
             [else
              ;;   --             ---        ---           ------
              ;; ------   or   ------   or   ------   or   ------
              (interval-list-subseteq? (rest I1) I2)])]))
  
  (: interval*-subseteq? (Maybe-Interval* Maybe-Interval* -> Boolean))
  (define (interval*-subseteq? I1 I2)
    (cond [(empty-set? I1)  #t]
          [(empty-set? I2)  #f]
          [(and (Interval? I1) (Interval? I2))  (interval-subseteq? I1 I2)]
          [else  (interval-list-subseteq?
                  (if (Interval? I1) (list I1) (Interval-List-elements I1))
                  (if (Interval? I2) (list I2) (Interval-List-elements I2)))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Other ops
  
  (: interval*-member? (Maybe-Interval* Flonum -> Boolean))
  (define (interval*-member? I x)
    (cond [(empty-set? I)  #f]
          [(Interval? I)   (interval-member? I x)]
          [else  (ormap (λ: ([I : Interval]) (interval-member? I x))
                        (Interval-List-elements I))]))
  
  (: interval*-sample-point (Interval* -> Flonum))
  (define (interval*-sample-point I)
    (cond [(Interval? I)  (interval-sample-point I)]
          [else
           (define Is (Interval-List-elements I))
           (define i (sample-index (normalize-probs/+2 (map/+2 interval-measure Is))))
           (interval-sample-point (list-ref Is i))]))
  
  (: interval*-measure (Maybe-Interval* -> Flonum))
  (define (interval*-measure I)
    (cond [(empty-set? I)  0.0]
          [(Interval? I)   (interval-measure I)]
          [else  (flsum (map interval-measure (Interval-List-elements I)))]))
  
  (: interval*-map/mono ((Interval -> Maybe-Interval*) Interval* Any -> Maybe-Interval*))
  (define (interval*-map/mono f I increasing?)
    (: Is (Listof Interval))
    (define Is
      (let: loop ([Is : (Listof Interval)  (if (Interval? I) (list I) (Interval-List-elements I))])
        (cond [(empty? Is)  empty]
              [else  (define I (f (first Is)))
                     (cond [(empty-set? I)  (loop (rest Is))]
                           [(Interval? I)   (cons I (loop (rest Is)))]
                           [else  (append (Interval-List-elements I) (loop (rest Is)))])])))
    (interval-list (if increasing? Is (reverse Is))))
  
  (: interval*-map ((Interval -> Maybe-Interval*) Interval* -> Maybe-Interval*))
  (define (interval*-map f I)
    (: Is (Listof Interval*))
    (define Is
      (let: loop ([Is : (Listof Interval)  (if (Interval? I) (list I) (Interval-List-elements I))])
        (cond [(empty? Is)  empty]
              [else  (define I (f (first Is)))
                     (cond [(empty-set? I)  (loop (rest Is))]
                           [else   (cons I (loop (rest Is)))])])))
    (foldr interval*-union empty-set Is))
  
  )  ; module type

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
  
  (define-syntax interval-list? (make-rename-transformer #'Interval-List?))
  (define-syntax interval-list-elements (make-rename-transformer #'Interval-List-elements))
  
  )  ; module

(require (submod "." typed-defs)
         (submod "." untyped-defs))
