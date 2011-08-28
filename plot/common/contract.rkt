#lang racket/base

(require racket/contract racket/draw racket/class
         (for-syntax racket/base racket/list
                     syntax/parse racket/syntax syntax/strip-context
                     "contract-stx.rkt")
         (prefix-in s: scribble/manual)
         (prefix-in s: scribble/core))

(provide (all-defined-out))

(define nonnegative-real/c (opt/c (and/c real? (>=/c 0))))
(define positive-real/c (opt/c (and/c real? (>/c 0))))
(define positive-integer/c (opt/c (and/c integer? (>/c 0))))

(define plot-color/c
  (opt/c (or/c (list/c nonnegative-real/c nonnegative-real/c nonnegative-real/c)
               symbol? string? (is-a?/c color%))))

(define brush-style/c
  (one-of/c 'transparent 'solid 'opaque 'xor 'hilite 'panel 'bdiagonal-hatch
            'crossdiag-hatch 'fdiagonal-hatch 'cross-hatch 'horizontal-hatch
            'vertical-hatch))

(define pen-style/c
  (one-of/c 'transparent 'solid 'xor 'hilite 'dot 'long-dash 'short-dash
            'dot-dash 'xor-dot 'xor-long-dash 'xor-short-dash 'xor-dot-dash))

(define font-family/c
  (one-of/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))

(define point-label/c (or/c string? symbol?))

(define color-function/c
  (real? real? positive-integer/c . -> . (listof plot-color/c)))

(define-syntax-rule (->/plot2d-kws arg/c ... ret/c)
  (->* (arg/c ...)
       (#:width positive-integer/c #:height positive-integer/c
        #:title (or/c string? #f)
        #:x-label (or/c string? #f)
        #:y-label (or/c string? #f)
        #:x-min (or/c real? #f) #:x-max (or/c real? #f)
        #:y-min (or/c real? #f) #:y-max (or/c real? #f))
       ret/c))

(define-syntax-rule (->/plot3d-kws args ... output)
  (->* (args ...)
       (#:width positive-integer/c #:height positive-integer/c
        #:angle real? #:altitude real?
        #:title (or/c string? #f)
        #:x-label (or/c string? #f)
        #:y-label (or/c string? #f)
        #:z-label (or/c string? #f)
        #:x-min (or/c real? #f) #:x-max (or/c real? #f)
        #:y-min (or/c real? #f) #:y-max (or/c real? #f)
        #:z-min (or/c real? #f) #:z-max (or/c real? #f))
       output))

(define-for-syntax (get-required-contract arg-stx)
  (syntax-parse arg-stx
    [(name:id contract:expr)             (list #'contract)]
    [(kw:keyword name:id contract:expr)  (list #'kw #'contract)]
    [_  empty]))

(define-for-syntax (get-optional-contract arg-stx)
  (syntax-parse arg-stx
    [(name:id contract:expr default:expr)             (list #'contract)]
    [(kw:keyword name:id contract:expr default:expr)  (list #'kw #'contract)]
    [_  empty]))

(define-for-syntax (remove-contract arg-stx)
  (syntax-parse arg-stx
    [(name:id contract:expr)               (list #'name)]
    [(name:id contract:expr default:expr)  (list #'(name default))]
    [(kw:keyword name:id contract:expr)    (list #'kw #'name)]
    [(kw:keyword name:id contract:expr default:expr)
     (list #'kw #'(name default))]))

(define-for-syntax (replace-context/no-head ctxt stx)
  (syntax-case stx ()
    [(head e ...)
     (quasisyntax/loc stx
       (head #,@(map (λ (e) (replace-context ctxt e))
                     (syntax->list #'(e ...)))))]))

(define-syntax (defproc stx)
  (syntax-parse stx
    [(_ (name:id arg:argument-spec ...) result-contract:expr
        body ...+)
     (define arg-list (syntax->list #'(arg ...)))
     (define/with-syntax name:doc (format-id #'name "~a:doc" #'name))
     (define/with-syntax (new-arg ...) (append* (map remove-contract arg-list)))
     (define/with-syntax (req-contract ...) (append* (map get-required-contract
                                                          arg-list)))
     (define/with-syntax (opt-contract ...) (append* (map get-optional-contract
                                                          arg-list)))
     (syntax/loc stx
       (begin
         (define/contract (name new-arg ...)
           (->* (req-contract ...)
                (opt-contract ...)
                result-contract)
           body ...)
         (define-syntax (name:doc inner-stx)
           (syntax-case inner-stx ()
             [(the-name pre-flow (... ...))
              (replace-context/no-head
               #'the-name (syntax/loc inner-stx
                            (s:defproc (name arg ...)
                                              result-contract
                                              pre-flow (... ...))))]))
         ))]))

(define-syntax (defparam stx)
  (syntax-parse stx
    [(_ name:id arg:id contract:expr default:expr)
     (define/with-syntax name:doc (format-id #'name "~a:doc" #'name))
     (syntax/loc stx
       (begin
         (define/contract name (parameter/c contract) (make-parameter default))
         (define-syntax (name:doc inner-stx)
           (syntax-case inner-stx ()
             [(name:doc pre-flow (... ...))
              (with-syntax
                  ([inner-name      (replace-context #'name:doc #'name)]
                   [inner-arg       (replace-context #'name:doc #'arg)]
                   [inner-contract  (replace-context #'name:doc #'contract)]
                   [inner-default   (replace-context #'name:doc #'default)])
                (syntax/loc inner-stx
                  (s:defparam
                   inner-name inner-arg inner-contract
                   (s:tabular #:style (s:make-style 'boxed '(div))
                    (list (list (s:racket
                                 (inner-name inner-default)))))
                   pre-flow (... ...))))]))
         ))]))
