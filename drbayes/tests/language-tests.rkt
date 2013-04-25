#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         "../main.rkt"
         "test-utils.rkt")

(printf "starting...~n")

(interval-max-splits 0)

(define/drbayes (S)
  (lazy-if ((uniform) . < . 0.5) #;(boolean (const 0.5)) (T) (F)))

(define/drbayes (tiny-unif)
  (uniform (const (flstep 1.0 -5)) (const (flstep 1.0 5))))

(define/drbayes (T)
  (lazy-cond [(racket (late-boolean/arr 0.4)) #;(boolean (const 0.5))  (cons #t (T))]
             ;[((uniform) . <= . 0.5)  (cons #t (F))]
             [else  null])
  #;
  (lazy-cond [((uniform) . <= . 0.4) #;(boolean (const 0.4))  (cons #t (T))]
             [((uniform) . <= . 0.5) #;(boolean (const 0.5))  (cons #t (F))]
             [else  null]))

(define/drbayes (F)
  (lazy-cond [((uniform) . < . 0.5) #;(boolean (const 0.5))  '(#f)]
             [else  null])
  #;
  (lazy-cond [((uniform) . <= . 0.4) #;(boolean (const 0.4))  (cons #f (F))]
             [((uniform) . <= . 0.5) #;(boolean (const 0.5))
              (cons #f (T))
              #;
              (cons #f (let ([s  (T)])
                         (prim-if (list-ref s (const 1))
                                  (fail)
                                  s)))]
             [else  null]))

#;
(drbayes-sample (drbayes (S)) 20)

(define-values (ss ws)
  (let ()
    (define sws
      (time
       (let ()
         (define-values (ss ws)
           (drbayes-sample (drbayes (T))
                           20000
                           #;
                           (set-list* booleans trues falses trues falses trues universe)))
         (map (inst cons Value Flonum) ss ws))))
    (values (map (inst car Value Flonum) sws)
            (map (inst cdr Value Flonum) sws))))

;ss
;ws

(: value-shape<? (Value Value -> Boolean))
(define (value-shape<? v1 v2)
  (cond [(null? v1)
         (cond [(null? v2)  #f]
               [else  #t])]
        [(boolean? v1)
         (cond [(null? v2)  #f]
               [(boolean? v2)  #f]
               [else  #t])]
        [(flonum? v1)
         (cond [(null? v2)  #f]
               [(boolean? v2)  #f]
               [(flonum? v2)  #f]
               [else  #t])]
        [(pair? v1)
         (cond [(or (null? v2) (boolean? v2) (flonum? v2))  #f]
               [(pair? v2)
                (cond [(value-shape<? (car v1) (car v2))  #t]
                      [(value-shape<? (car v2) (car v1))  #f]
                      [else  (value-shape<? (cdr v1) (cdr v2))])]
               [else  #t])]
        [else
         (cond [(or (null? v2) (boolean? v2) (flonum? v2))  #f]
               [(pair? v2)  #f]
               [else
                (define t1 (symbol->string (get-tag v1)))
                (define t2 (symbol->string (get-tag v2)))
                (cond [(string<? t1 t2)  #t]
                      [(string<? t2 t1)  #f]
                      [else  (value-shape<? (get-val v1) (get-val v2))])])]))

(: value-elements<? (Value Value -> Boolean))
(define (value-elements<? v1 v2)
  (cond [(and (null? v1) (null? v2))  #f]
        [(and (boolean? v1) (boolean? v2))  (and (not v1) v2)]
        [(and (flonum? v1) (flonum? v2))  (< v1 v2)]
        [(and (pair? v1) (pair? v2))
         (cond [(value-elements<? (car v1) (car v2))  #t]
               [(value-elements<? (car v2) (car v1))  #f]
               [else  (value-elements<? (cdr v1) (cdr v2))])]
        [(and (tagged? v1) (tagged? v2))
         (define t1 (symbol->string (get-tag v1)))
         (define t2 (symbol->string (get-tag v2)))
         (cond [(string<? t1 t2)  #f]
               [(string<? t2 t1)  #f]
               [else  (value-elements<? (get-val v1) (get-val v2))])]
        [else  #f]))
               
(: value<? (Value Value -> Boolean))
(define (value<? v1 v2)
  (cond [(value-shape<? v1 v2)  #t]
        [(value-shape<? v2 v1)  #f]
        [else  (value-elements<? v1 v2)]))

(let*-values ([(ss ws)  (count-samples ss ws)]
              [(ss ws)  (let ([d  (discrete-dist ss ws)])
                          (values (discrete-dist-values d) (discrete-dist-probs d)))])
  (sort-samples value<? ss ws))

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
