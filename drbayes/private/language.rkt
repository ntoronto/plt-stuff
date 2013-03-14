#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/math
                     syntax/parse
                     syntax/strip-context
                     racket/syntax
                     racket/string)
         racket/list
         racket/math
         racket/promise
         (for-template "functions.rkt"
                       racket/base
                       racket/list
                       racket/math)
         (except-in typed/racket/base Refinement)
         racket/stxparam
         "functions.rkt"
         "set.rkt"
         "arrow.rkt")

(provide define/drbayes drbayes racket)

(begin-for-syntax
  (struct first-order-function (transformer) #:property prop:procedure 0)
  (struct bound-identifier (transformer) #:property prop:procedure 0)
  
  (define (inexact-constant orig-stx)
    (let loop ([stx orig-stx])
      (define e (if (syntax? stx) (syntax-e stx) stx))
      (cond [(pair? e)     (cons (loop (car e)) (loop (cdr e)))]
            [(null? e)     null]
            [(real? e)     (real->double-flonum e)]
            [(boolean? e)  e]
            [(symbol? e)   e]
            [else
             (raise-syntax-error 'drbayes "unimplemented constant" orig-stx stx)])))
  
  (define-syntax-class real
    #:description "real number"
    (pattern x #:when (real? (syntax->datum #'x))))
  
  (define-syntax-class constant
    #:description "literal constant"
    #:attributes (expression)
    #:literals (quote null empty)
    (pattern (~or null empty) #:attr expression #'null/exp)
    (pattern (~or x:boolean x:number x:str x:char (quote x))
             #:attr expression #`(c/arr (quote #,(inexact-constant #'x))))
    )
  
  (define-syntax-class primitive-transformer
    #:description "primitive syntax transformer"
    #:attributes (expression)
    #:literals (and or not)
    (pattern (or) #:attr expression #'#f)
    (pattern (or e:expr) #:attr expression #'e)
    (pattern (or e0:expr es:expr ...) #:attr expression #'(strict-if e0 #t (or es ...)))
    (pattern (and) #:attr expression #'#t)
    (pattern (and e:expr) #:attr expression #'e)
    (pattern (and e0:expr es:expr ...) #:attr expression #'(strict-if e0 (and es ...) #f))
    (pattern (not e:expr) #:attr expression #'(strict-if e #f #t))
    )
  
  (define-syntax-class 0ary-primitive
    #:description "zero-ary primitive operator"
    #:attributes (combinator)
    #:literals (fail)
    (pattern fail #:attr combinator #'(λ () bottom/arr))
    )
  
  (define-syntax-class 1ary-primitive
    #:description "unary primitive operator"
    #:attributes (combinator)
    #:literals (car cdr real? null? pair? boolean?
                    exp log sqr sqrt
                    negative? positive? nonnegative? nonpositive?)
    (pattern car #:attr combinator #'fst/exp)
    (pattern cdr #:attr combinator #'snd/exp)
    (pattern real? #:attr combinator #'real?/exp)
    (pattern null? #:attr combinator #'null?/exp)
    (pattern pair? #:attr combinator #'pair?/exp)
    (pattern boolean? #:attr combinator #'boolean?/exp)
    (pattern exp #:attr combinator #'exp/exp)
    (pattern log #:attr combinator #'log/exp)
    (pattern sqr #:attr combinator #'sqr/exp)
    (pattern sqrt #:attr combinator #'sqrt/exp)
    (pattern negative? #:attr combinator #'negative?/exp)
    (pattern positive? #:attr combinator #'positive?/exp)
    (pattern nonnegative? #:attr combinator #'nonnegative?/exp)
    (pattern nonpositive? #:attr combinator #'nonpositive?/exp)
    )
  
  (define-syntax-class 2ary-primitive
    #:description "binary primitive operator"
    #:attributes (combinator)
    #:literals (cons + * < <= > >=)
    (pattern cons #:attr combinator #'pair/exp)
    (pattern + #:attr combinator #'+/exp)
    (pattern * #:attr combinator #'*/exp)
    (pattern < #:attr combinator #'lt/exp)
    (pattern <= #:attr combinator #'lte/exp)
    (pattern > #:attr combinator #'gt/exp)
    (pattern >= #:attr combinator #'gte/exp)
    )
  
  (define-syntax-class primitive
    #:description "primitive operator"
    #:attributes (combinator)
    #:literals (list uniform normal cauchy)
    (pattern list #:attr combinator #'list/exp)
    (pattern uniform #:attr combinator #'uniform/exp)
    (pattern normal #:attr combinator #'normal/exp)
    (pattern cauchy #:attr combinator #'cauchy/exp)
    )
  
  (define-syntax-class primitive-apply
    #:description "primitive application"
    #:attributes (combinator [args 1])
    #:literals (- /)
    (pattern (~and (- arg) (- args ...)) #:attr combinator #'neg/exp)
    (pattern (~and (- arg0 arg1) (- args ...)) #:attr combinator #'-/exp)
    (pattern (~and e (- args ...))
             #:when (raise-syntax-error 'drbayes "expected 1 or 2 arguments" #'e)
             #:attr combinator #'(λ () bottom/arr))
    
    (pattern (~and (/ arg) (/ args ...)) #:attr combinator #'recip/exp)
    (pattern (~and (/ arg0 arg1) (/ args ...)) #:attr combinator #'//exp)
    (pattern (~and e (/ args ...))
             #:when (raise-syntax-error 'drbayes "expected 1 or 2 arguments" #'e)
             #:attr combinator #'(λ () bottom/arr))
    
    (pattern (~and e (op:0ary-primitive ~! args ...))
             #:when (if (= 0 (length (syntax->list #'(args ...))))
                        #t
                        (raise-syntax-error 'drbayes "expected 0 arguments" #'e))
             #:attr combinator #'op.combinator)
    
    (pattern (~and e (op:1ary-primitive ~! args ...))
             #:when (if (= 1 (length (syntax->list #'(args ...))))
                        #t
                        (raise-syntax-error 'drbayes "expected 1 argument" #'e))
             #:attr combinator #'op.combinator)
    
    (pattern (~and e (op:2ary-primitive ~! args ...))
             #:when (if (= 2 (length (syntax->list #'(args ...))))
                        #t
                        (raise-syntax-error 'drbayes "expected 2 arguments" #'e))
             #:attr combinator #'op.combinator)
    
    (pattern (op:primitive args ...) #:attr combinator #'op.combinator)
    )
  
  (define-syntax-class let-binding
    #:description "[id  expr]"
    #:attributes (id expr)
    (pattern [id:id expr:expr]))
  
  (define-syntax-class boolean-expr-not-else
    #:description "boolean-expr (not else)"
    #:literals (else)
    (pattern (~and e:expr (~not else))))
  
  (define-syntax-class cond-case
    #:description "[boolean-expr  then-expr]"
    #:attributes (cond then)
    (pattern [(~and cond:expr _:boolean-expr-not-else) then:expr]))
  
  (define-syntax-class cond-else
    #:description "[else  else-expr]"
    #:attributes (expr)
    #:literals (else)
    (pattern [else  expr:expr]))
  
  (define-syntax-class if-expr
    #:description "(if boolean-expr then-expr else-expr)"
    #:attributes (cond then else)
    (pattern (_ cond:expr then:expr else:expr)))
  
  )  ; begin-for-syntax

(define-syntax-parameter in-drbayes? #f)
(define-syntax-parameter let-depth 0)

;; A `let' is transformed (partly) into a `let-syntax' with the result of calling this function as
;; its value
(define-for-syntax make-binding-transformer
  (case-lambda
    [(stx old-d)
     (with-syntax ([old-d old-d])
       (syntax/loc stx
         (bound-identifier
          (λ (inner-stx)
            (cond [(syntax-parameter-value #'in-drbayes?)
                   (define d (syntax-parameter-value #'let-depth))
                   (define i (quasisyntax/loc inner-stx (env/exp #,(- d old-d))))
                   (syntax-case inner-stx () [(_ . args)  #`(#,i . args)] [_  i])]
                  [else
                   (raise-syntax-error 'drbayes "reference to DrBayes binding in Racket code"
                                       inner-stx)])))))]
    [(stx old-d idx)
     (with-syntax ([old-d old-d] [idx idx])
       (syntax/loc stx
         (bound-identifier
          (λ (inner-stx)
            (cond [(syntax-parameter-value #'in-drbayes?)
                   (define d (syntax-parameter-value #'let-depth))
                   (define i (quasisyntax/loc inner-stx (list-ref/exp (env/exp #,(- d old-d)) idx)))
                   (syntax-case inner-stx () [(_ . args)  #`(#,i . args)] [_  i])]
                  [else
                   (raise-syntax-error 'drbayes "reference to DrBayes binding in Racket code"
                                       inner-stx)])))))]))

(define-for-syntax (raise-syntax-arity-error name arity stx)
  (define arguments (if (= arity 1) "argument" "arguments"))
  (raise-syntax-error name (format "expected ~a ~a" arity arguments) stx))

(define-syntax (define/drbayes stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     (syntax/loc stx
       (define name (drbayes body)))]
    [(_ (name:id arg:id ...) body:expr)
     (define arity (length (syntax->list #'(arg ...))))
     (define/with-syntax (value ...)
       (build-list arity (λ (idx) (make-binding-transformer stx 1 idx))))
     (define/with-syntax value-name (generate-temporary #'name))
     (quasisyntax/loc stx
       (begin
         (: value-name expression)
         (define value-name
           (let-syntax ([arg  value] ...)
             (syntax-parameterize ([in-drbayes?  #t] [let-depth  1])
               (parse body))))
         
         (define-syntax name
           (first-order-function
            (λ (inner-stx)
              (cond [(syntax-parameter-value #'in-drbayes?)
                     (syntax-case inner-stx ()
                       [(_ arg ...)
                        (syntax/loc inner-stx
                          (apply/exp value-name (list (parse arg) ...)))]
                       [(_ e (... ...))
                        (raise-syntax-arity-error 'drbayes #,arity inner-stx)]
                       [_
                        (raise-syntax-error 'drbayes "expected function application" inner-stx)])]
                    [else
                     (raise-syntax-error 'drbayes "reference to DrBayes function in Racket code"
                                         inner-stx)]))))))]))

(define-syntax (parse stx)
  ;; Make sure `stx' has the source location of the inner expression; without this, there would be a
  ;; lot of (quasisyntax/loc stx (... #,(syntax/loc #'e (parse e)) ...)) in the following code
  (let ([stx  (syntax-case stx () [(_ e)  (syntax/loc #'e (parse e))])])
    (syntax-parse stx #:literals (const
                                  racket
                                  lazy
                                  prim-if prim-cond
                                  strict-if strict-cond
                                  lazy-if lazy-cond
                                  let let*
                                  list-ref scale translate boolean
                                  tag? tag untag)
      [(_ e:constant)
       (syntax/loc stx e.expression)]
      
      [(_ e:primitive-transformer)
       (syntax/loc stx (parse e.expression))]
      
      [(_ e:primitive-apply)
       (syntax/loc stx (e.combinator (parse e.args) ...))]
      
      [(_ (const e:expr))
       (syntax/loc stx (c/arr (const e #'e)))]
      
      [(_ (racket e:expr))
       (syntax/loc stx
         (let ([v  (racket e)])
           (cond [(expression? v)  v]
                 [else  (raise-syntax-error 'drbayes "does not evaluate to a DrBayes expression"
                                            #'e)])))]
      
      [(_ (lazy e:expr))
       (syntax/loc stx (delay/exp (λ () (parse e))))]
      
      [(_ (~and (prim-if . _) ~! e:if-expr))
       (syntax/loc stx (prim-if/arr (parse e.cond) (parse e.then) (parse e.else)))]
      
      [(_ (~and (lazy-if . _) ~! e:if-expr))
       (syntax/loc stx (lazy-if/exp (parse e.cond) (λ () (parse e.then)) (λ () (parse e.else))))]
      
      [(_ (~and (strict-if . _) ~! e:if-expr))
       (syntax/loc stx (strict-if/exp (parse e.cond) (λ () (parse e.then)) (λ () (parse e.else))))]
      
      [(_ (~and (prim-cond _) ~! (prim-cond e:cond-else)))
       (syntax/loc stx (parse e.expr))]
      
      [(_ (~and (prim-cond . _) ~! (prim-cond c0:cond-case c:cond-case ... ~! e:cond-else)))
       (syntax/loc stx (parse (prim-if c0.cond c0.then (prim-cond c ... e))))]
      
      [(_ (~and (lazy-cond _) ~! (lazy-cond e:cond-else)))
       (syntax/loc stx (parse e.expr))]
      
      [(_ (~and (lazy-cond . _) ~! (lazy-cond c0:cond-case c:cond-case ... ~! e:cond-else)))
       (syntax/loc stx (parse (lazy-if c0.cond c0.then (lazy-cond c ... e))))]
      
      [(_ (~and (strict-cond _) ~! (strict-cond e:cond-else)))
       (syntax/loc stx (parse e.expr))]
      
      [(_ (~and (strict-cond . _) ~! (strict-cond c0:cond-case c:cond-case ... ~! e:cond-else)))
       (syntax/loc stx (parse (strict-if c0.cond c0.then (strict-cond c ... e))))]
      
      [(_ (list-ref ~! e:expr (const i:expr)))
       (syntax/loc stx (list-ref/exp (parse e) (cast i Natural)))]
      
      [(_ (scale ~! e:expr (const x:expr)))
       (syntax/loc stx (scale/exp (parse e) (cast (const x #'x) Flonum)))]
      
      [(_ (translate ~! e:expr (const x:expr)))
       (syntax/loc stx (translate/exp (parse e) (cast (const x #'x) Flonum)))]
      
      [(_ (boolean ~! (const p:expr)))
       (syntax/loc stx (boolean/exp (cast (const p #'p) Flonum)))]
      
      [(_ (tag? ~! e:expr t:expr))
       (syntax/loc stx (tag?/exp (parse e) t))]
      
      [(_ (tag ~! e:expr t:expr))
       (syntax/loc stx (tag/exp (parse e) t))]
      
      [(_ (untag ~! e:expr t:expr))
       (syntax/loc stx (untag/exp (parse e) t))]
      
      [(_ (let () body:expr))
       (syntax/loc stx (parse body))]
      
      [(_ (let (b:let-binding) body:expr))
       (define d (+ 1 (syntax-parameter-value #'let-depth)))
       (define/with-syntax let-depth+1 d)
       (define/with-syntax value (make-binding-transformer stx d))
       (syntax/loc stx
         (let/exp (parse b.expr)
                  (let-syntax ([b.id  value])
                    (syntax-parameterize ([let-depth  let-depth+1])
                      (parse body)))))]
      
      [(_ (let ~! (b:let-binding ...) body:expr))
       (define d (+ 1 (syntax-parameter-value #'let-depth)))
       (define/with-syntax let-depth+1 d)
       (define/with-syntax (value ...)
         (build-list (length (syntax->list #'(b.id ...)))
                     (λ (idx) (make-binding-transformer stx d idx))))
       (syntax/loc stx
         (let/exp (parse (list b.expr ...))
                  (let-syntax ([b.id  value] ...)
                    (syntax-parameterize ([let-depth  let-depth+1])
                      (parse body)))))]
      
      [(_ (let* () body:expr))
       (syntax/loc stx (parse body))]
      
      [(_ (let* ~! (b0:let-binding b:let-binding ...) body:expr))
       (syntax/loc stx
         (parse (let ([b0.id b0.expr]) (let* (b ...) body))))]
      
      [(_ (x:id args ...))
       #:when (bound-identifier? (syntax-local-value #'x (λ () #f)))
       (raise-syntax-error (syntax->datum #'x) "expected function" stx #'x)]
      
      [(_ (f:id args ...))
       #:when (first-order-function? (syntax-local-value #'f (λ () #f)))
       (syntax/loc stx (f args ...))]
      
      [(_ (f:id . args))
       #:when (let ([proc  (syntax-local-value #'f (λ () #f))])
                (and (procedure? proc) (procedure-arity-includes? proc 1)))
       (raise-syntax-error 'drbayes "macro expansion not supported" (syntax/loc stx (f:id . args)))]
      
      [(_ f:id)
       #:when (first-order-function? (syntax-local-value #'f (λ () #f)))
       (raise-syntax-error (syntax->datum #'f) "expected function application" stx #'f)]
      
      [(_ x:id)
       #:when (bound-identifier? (syntax-local-value #'x (λ () #f)))
       (syntax/loc stx x)]
      
      [(_ e)
       (raise-syntax-error 'drbayes "unrecognized syntax" stx #'e)]
      )))

(define-syntax (drbayes stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx
       (syntax-parameterize ([in-drbayes? #t])
         (parse e)))]))

(define-syntax (racket stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx
       (syntax-parameterize ([in-drbayes? #f])
         e))]))
