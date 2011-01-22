#lang racket/base

;; todo: unzip integers like vectors of bits?

(require racket/match racket/list
         (rename-in racket/vector [vector-copy racket-vector-copy])
         (for-syntax racket/base unstable/syntax))

(provide (except-out (all-defined-out) vector-copy))

(define vector-copy
  (case-lambda
    [(v)      (racket-vector-copy v)]
    [(v s)    (if (= s (vector-length v))
                  (make-vector 0)
                  (racket-vector-copy v s))]
    [(v s e)  (if (= s e (vector-length v))
                  (make-vector 0)
                  (racket-vector-copy v s e))]))

;; zipper
;; Has 1. path : (listof (B -> A)), a path from the zipper back to the root,
;;        which is of type A
;;     2. value : B, the value currently in focus
(struct zipper (path value) #:transparent)

(define (new-zipper value) (zipper null value))

;; define-unzip
;; Given a getter and a functional setter, defines an unzip function
(define-syntax (define-unzip stx)
  (syntax-case stx ()
    [(_ unzip field-ref field-set)
     (identifier? #'unzip)
     (syntax/loc stx
       (define (unzip z)
         (match-let ([(zipper path v)  z])
           (let ([field-set  (λ (e) (field-set v e))])
             (zipper (cons field-set path) (field-ref v))))))]))

;; zip : zipper [nonnegative-integer] -> zipper
;; Applies the functional updates in the path to zipper-value, and returns
;; a new zipper with a shorter path
(define zip
  (case-lambda
    [(z)
     (match-let ([(zipper path v)  z])
       (if (empty? path)
           (error 'zip "expected a zipper with a non-empty path, got ~e" z)
           (match-let ([(cons field-set path)  path])
             (zipper path (field-set v)))))]
    [(z n)
     (match-let ([(zipper path v)  z])
       (when ((length path) . < . n)
         (error 'zip "expected a zipper with path length >= ~a, got one with path length ~a: ~e"
                n (length path) z))
       (let loop ([path path] [v v] [n n])
         (if (zero? n)
             (zipper path v)
             (match-let ([(cons field-set path)  path])
               (loop path (field-set v) (sub1 n))))))]))

;; zip* : zipper -> zipper
;; Zips a zipper all the way
(define (zip* z)
  (match-let ([(zipper path v)  z])
    (let loop ([path path] [v v])
      (if (null? path)
          (zipper path v)
          (match-let ([(cons field-set path)  path])
            (loop path (field-set v)))))))

(define (zipper->value z)
  (zipper-value (zip* z)))

;; zipper-set : zipper any -> zipper
;; Functionally updates a zipper's current value
(define (zipper-set z value)
  (zipper (zipper-path z) value))

;; =============================================================================
;; unzippers for built-in data types

;; conses

(define (unzip-car z)
  (match-let ([(zipper path (cons a b))  z])
    (let ([cons-car  (λ (a) (cons a b))])
      (zipper (cons cons-car path) a))))

(define (unzip-cdr z)
  (match-let ([(zipper path (cons a b))  z])
    (let ([cons-cdr  (λ (b) (cons a b))])
      (zipper (cons cons-cdr path) b))))

;; proper lists

(define (unzip-first z)
  (match-let ([(zipper path (list a b ...))  z])
    (let ([cons-first  (λ (a) (cons a b))])
      (zipper (cons cons-first path) a))))

(define (unzip-rest z)
  (match-let ([(zipper path (list a b ...))  z])
    (let ([cons-rest  (λ (b) (cons a b))])
      (zipper (cons cons-rest path) b))))

;; vectors

(define (unzip-vector z i)
  (match-let ([(zipper path v)  z])
    (let* ([new-v       (vector-copy v)]
           [vector-set  (λ (e) (vector-set! new-v i e) new-v)])
      (zipper (cons vector-set path) (vector-ref v i)))))

(define (unzip-vector-left z n)
  (match-let ([(zipper path v)  z])
    (let* ([right           (vector-copy v n)]
           [vector-prepend  (λ (left) (vector-append left right))])
      (zipper (cons vector-prepend path) (vector-copy v 0 n)))))

(define (unzip-vector-right z n)
  (match-let ([(zipper path v)  z])
    (let* ([left             (vector-copy v 0 n)]
           [vector-postpend  (λ (right) (vector-append left right))])
      (zipper (cons vector-postpend path) (vector-copy v n)))))

(define (unzip-subvector z start end)
  (match-let ([(zipper path v)  z])
    (let* ([left           (vector-copy v 0 start)]
           [right          (vector-copy v end)]
           [vector-insert  (λ (mid) (vector-append left mid right))])
      (zipper (cons vector-insert path) (vector-copy v start end)))))

;; strings

(define (unzip-string z i)
  (match-let ([(zipper path s)  z])
    (let* ([new-s       (string-copy s)]
           [string-set  (λ (c) (string-set! new-s i c) new-s)])
      (zipper (cons string-set path) (string-ref s i)))))

(define (unzip-string-left z n)
  (match-let ([(zipper path str)  z])
    (let* ([right  (substring str n)]
           [string-prepend  (λ (left) (string-append left right))])
      (zipper (cons string-prepend path) (substring str 0 n)))))

(define (unzip-string-right z n)
  (match-let ([(zipper path str)  z])
    (let* ([left  (substring str 0 n)]
           [string-postpend  (λ (right) (string-append left right))])
      (zipper (cons string-postpend path) (substring str n)))))

(define (unzip-substring z start end)
  (match-let ([(zipper path s)  z])
    (let* ([left  (substring s 0 start)]
           [right  (substring s end)]
           [string-insert  (λ (mid) (string-append left mid right))])
      (zipper (cons string-insert path) (substring s start end)))))

;; =============================================================================
;; unzippers for structs

;; define-struct-unzips
;; Defines unzip functions for the given fields of the given struct
;; Should work everywhere struct-copy works
(define-syntax (define-struct-unzips stx)
  (syntax-case stx ()
    [(_ struct-id (field ...))
     (and (identifier? #'struct-id)
          (andmap identifier? (syntax->list #'(field ...))))
     (with-syntax ([(unzip-field ...)
                    (map (λ (field)
                           (format-id field "unzip-~a-~a" #'struct-id field))
                         (syntax->list #'(field ...)))]
                   [(field-set ...)
                    (map (λ (field)
                           (format-id field "~a-~a-set" #'struct-id field))
                         (syntax->list #'(field ...)))]
                   [(field-ref ...)
                    (map (λ (field)
                           (format-id field "~a-~a" #'struct-id field))
                         (syntax->list #'(field ...)))])
       (syntax/loc stx
         (begin (define (unzip-field z)
                  (match-let ([(zipper path v)  z])
                    (let ([field-set  (λ (e)
                                        (struct-copy struct-id v [field e]))])
                      (zipper (cons field-set path) (field-ref v)))))
                ...)))]))
