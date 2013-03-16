#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         "../main.rkt")

(printf "starting...~n")

(define/drbayes (S)
  (lazy-if (boolean (const 0.5)) (T) (F)))

(define/drbayes (T)
  (lazy-cond [(boolean (const 0.4))  (cons #t (T))]
             [(boolean (const 0.5))  (cons #t (F))]
             [else  null]))

(define/drbayes (F)
  (lazy-cond [(boolean (const 0.4))  (cons #f (F))]
             [(boolean (const 0.5))  #;(cons #f (T))
                                     (cons #f (let ([s  (T)])
                                                  (prim-if (list-ref s (const 1))
                                                           (fail)
                                                           s)))]
             [else  null]))

#;
(drbayes-sample (drbayes (S)) 20)

(time
 (let ()
   (define-values (ss ws)
     (drbayes-sample (drbayes (S))
                     200
                     (set-list* booleans trues falses trues falses trues universe)))
   (list ss ws)))

(printf "search stats:~n")
(get-search-stats)
(newline)

(printf "cache stats:~n")
(get-cache-stats)
(newline)

#;; Racket version of the above, using rejection sampling
(let ()
  (: racket-S (-> (U #f (Listof Boolean))))
  (define (racket-S)
    (if ((random) . < . 0.5) (racket-T) (racket-F)))
  
  (: racket-T (-> (U #f (Listof Boolean))))
  (define (racket-T)
    (cond [((random) . < . 0.4)  (let ([s  (racket-T)])
                                   (and s (cons #t s)))]
          [((random) . < . 0.5)  (let ([s  (racket-F)])
                                   (and s (cons #t s)))]
          [else  null]))
  
  (: racket-F (-> (U #f (Listof Boolean))))
  (define (racket-F)
    (cond [((random) . < . 0.4)  (let ([s  (racket-F)])
                                   (and s (cons #f s)))]
          [((random) . < . 0.5)  (let ([s  (racket-T)])
                                   (and s
                                        (or (empty? s) (empty? (rest s)) (not (list-ref s 1)))
                                        (cons #f s)))]
          [else  null]))
  
  (time
   (let: loop : (Listof (Listof Boolean)) ([i : Nonnegative-Fixnum  0])
     (cond [(i . < . 200)
            (define s (racket-S))
            (match s
              [(list _ #t #f #t #f #t _ ...)  (cons (assert s pair?) (loop (+ i 1)))]
              ;[_  (cons (cast s (Listof Boolean)) (loop (+ i 1)))]
              [_  (loop i)])]
           [else
            empty])))
  )
