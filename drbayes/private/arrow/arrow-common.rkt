#lang typed/racket/base

(provide (all-defined-out))

(require racket/list
         "../untyped-utils.rkt")

(define-singleton-type Empty-Meaning empty-meaning)
(define-singleton-type Empty-Preimage empty-preimage)

(define rand-cache-preimages? #t)
(define rand-cache-computations? #f)
(define prim-cache-preimages? #t)
(define prim-cache-computations? #t)

(define rand-preimage-stats? #f)
(define rand-computation-stats? #f)
(define prim-preimage-stats? #f)
(define prim-computation-stats? #f)

(define: cache-stats : (HashTable Symbol Natural)  (make-hasheq empty))

(: increment-cache-stat (Symbol -> Void))
(define (increment-cache-stat name)
  (hash-set! cache-stats name (+ 1 (hash-ref cache-stats name (λ () 0)))))

(: get-cache-stats (-> (Listof (Pair Symbol Natural))))
(define (get-cache-stats)
  ((inst sort (Pair Symbol Natural) String)
   (hash-map cache-stats (λ: ([k : Symbol] [v : Natural]) (cons k v)))
   string<?
   #:key (λ: ([kv : (Pair Symbol Natural)]) (symbol->string (car kv)))
   #:cache-keys? #t))
