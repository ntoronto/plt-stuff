#lang typed/racket/base

(require (for-syntax racket/base racket/syntax)
         "utils.rkt" "bf.rkt")

(provide with-bf-bits define-bf-constant)

(define-syntax (with-bf-bits stx)
  (syntax-case stx ()
    [(_ bits e f ...)
     (syntax/loc stx
       (bfnormalize (parameterize ([bf-bits  bits])
                      e f ...)))]))

(define-syntax (define-bf-constant stx)
  (syntax-case stx ()
    [(_ name e0 e ...)
     (with-syntax ([name-cache  (format-id #'name "~a-cache"
                                           (syntax->datum #'name))]
                   [name-clear  (format-id #'name "~a-clear"
                                           (syntax->datum #'name))])
       (syntax/loc stx
         (begin
           (: name-cache (HashTable Integer bigfloat))
           (def name-cache (make-hash))
           (: name (-> bigfloat))
           (define (name)
             ; index cache by ceiling(log2(bits))
             (def log2-bits (max 6 (ceiling-log2i (bf-bits))))
             (bfnormalize
              (hash-ref!
               name-cache log2-bits
               (λ ()
                 ; 10 extra bits to keep rounding and double rounding
                 ; (due to bfnormalize) from being detectable
                 (parameterize ([bf-bits  (+ 10 (2^ log2-bits))])
                   e0 e ...)))))
           (: name-clear (-> Void))
           (define (name-clear)
             (set! name-cache
                   (ann (make-hash) (HashTable Integer bigfloat)))))))]))
