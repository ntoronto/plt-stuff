#lang racket/base

(require racket/contract racket/draw racket/class
         (for-syntax racket/base racket/list syntax/parse racket/syntax syntax/strip-context
                     "contract-stx.rkt")
         (prefix-in scribble. scribble/manual)
         (prefix-in scribble. scribble/core))

(provide (all-defined-out))

(define-syntax-rule (real>=/c r) (and/c real? (>=/c r)))
(define-syntax-rule (integer>=/c i) (and/c integer? (>=/c i)))

;; ===================================================================================================
;; Plot-specific contracts

(define anchor/c (one-of/c 'top-left 'top 'top-right
                           'left 'center 'right
                           'bottom-left 'bottom 'bottom-right))

(define plain-color/c (or/c (list/c (real>=/c 0) (real>=/c 0) (real>=/c 0))
                            string?
                            (is-a?/c color%)))

(define plot-color/c (or/c plain-color/c integer?))

(define line-style/c (or/c (one-of/c 'transparent 'solid 'dot 'long-dash 'short-dash 'dot-dash)
                           integer?))

(define fill-style/c (or/c (one-of/c 'transparent 'solid 'bdiagonal-hatch 'crossdiag-hatch
                                     'fdiagonal-hatch 'cross-hatch 'horizontal-hatch 'vertical-hatch)
                           integer?))

(define font-size/c (real>=/c 0))
(define font-family/c (one-of/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))

(define point-symbol/c (or/c string? symbol?))

(define color-function/c ((listof real?) . -> . (listof plot-color/c)))
(define line-style-function/c ((listof real?) . -> . (listof line-style/c)))
(define line-width-function/c ((listof real?) . -> . (listof (real>=/c 0))))
(define fill-style-function/c ((listof real?) . -> . (listof fill-style/c)))
(define alpha-function/c ((listof real?) . -> . (listof (real-in 0 1))))

(define plot-colors/c (or/c (listof plot-color/c) color-function/c))
(define line-styles/c (or/c (listof line-style/c) line-style-function/c))
(define line-widths/c (or/c (listof (real>=/c 0)) line-width-function/c))
(define fill-styles/c (or/c (listof fill-style/c) fill-style-function/c))
(define alphas/c (or/c (listof (real-in 0 1)) alpha-function/c))

;; ===================================================================================================
;; Definitions with contracts and contract documentation

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
    [(name:id contract:expr)                          (list #'name)]
    [(name:id contract:expr default:expr)             (list #'(name default))]
    [(kw:keyword name:id contract:expr)               (list #'kw #'name)]
    [(kw:keyword name:id contract:expr default:expr)  (list #'kw #'(name default))]))

(define-for-syntax (replace-context/no-head ctxt stx)
  (syntax-case stx ()
    [(head e ...)
     (quasisyntax/loc stx
       (head #,@(map (λ (e) (replace-context ctxt e))
                     (syntax->list #'(e ...)))))]))

(define-syntax (defproc stx)
  (syntax-parse stx
    [(_ (name:id arg:argument-spec ...) result-contract:expr body ...+)
     (define arg-list (syntax->list #'(arg ...)))
     (define/with-syntax name:doc (format-id #'name "~a:doc" #'name))
     (define/with-syntax (new-arg ...) (append* (map remove-contract arg-list)))
     (define/with-syntax (req-contract ...) (append* (map get-required-contract arg-list)))
     (define/with-syntax (opt-contract ...) (append* (map get-optional-contract arg-list)))
     (syntax/loc stx
       (begin
         (define/contract (name new-arg ...) (->* (req-contract ...) (opt-contract ...)
                                                  result-contract)
           body ...)
         (define-syntax (name:doc inner-stx)
           (syntax-case inner-stx ()
             [(the-name pre-flow (... ...))
              (replace-context/no-head
               #'the-name (syntax/loc inner-stx
                            (scribble.defproc (name arg ...) result-contract
                                              pre-flow (... ...))))]))))]))

(define-for-syntax (parameter-name->arg-name name-stx)
  (define name-str (symbol->string (syntax->datum name-stx)))
  (define arg-name-str
    (cond [(regexp-match #rx".*-(.*)$" name-str)
           => (λ (m) (last m))]
          [(regexp-match #rx"^$" name-str)
           => (λ (m) "value")]
          [else  (substring name-str 0 1)]))
  (format-id name-stx "~a" arg-name-str))

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
                  (scribble.defparam
                   inner-name inner-arg inner-contract
                   (scribble.tabular #:style (scribble.make-style 'boxed '(div))
                                     (list (list (scribble.racket
                                                  (inner-name inner-default)))))
                   pre-flow (... ...))))]))))]
    [(_ name:id contract:expr default:expr)
     (define/with-syntax arg-name (parameter-name->arg-name #'name))
     (syntax/loc stx (defparam name arg-name contract default))]))
