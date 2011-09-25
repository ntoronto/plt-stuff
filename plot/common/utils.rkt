#lang racket/base

(require racket/sequence racket/list)

(provide sequence-take list-index assoc-cons)


(define (sequence-take seq start end)
  (for/list ([e  (sequence-tail seq start)] 
             [_  (in-range (- end start))])
    e))

(define (list-index v lst [equal? equal?])
  (let loop ([lst lst] [idx 0])
    (cond [(null? lst)  -1]
          [(equal? v (car lst))  idx]
          [else  (loop (cdr lst) (add1 idx))])))

(define (assoc-cons hash key new-value)
  (let loop ([hash  hash])
    (cond [(empty? hash)  (list (cons key (list new-value)))]
          [else
           (define entry (first hash))
           (cond [(equal? (car entry) key)  (cons (cons key (cons new-value (cdr entry)))
                                                  (rest hash))]
                 [else  (cons (first hash) (loop (rest hash)))])])))
