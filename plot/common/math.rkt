#lang racket/base

(require racket/math racket/flonum racket/match racket/list racket/sequence)

(provide (all-defined-out))

(define -pi (- pi))
(define 2pi (* 2 pi))
(define -1/2pi (* -1/2 pi))
(define 1/2pi (* 1/2 pi))

(define (real-modulo x y) (- x (* y (floor (/ x y)))))

(define (flmodulo x y) (fl- x (fl* y (flfloor (fl/ x y)))))
(define (fldist2 x y) (flsqrt (fl+ (fl* x x) (fl* y y))))
(define (fldist3 x y z) (flsqrt (fl+ (fl* x x) (fl+ (fl* y y) (fl* z z)))))

(define (alpha-blend x1 x2 a)
  (+ (* a x1) (* (- 1 a) x2)))

(define (build-linear-seq start step num)
  (for/list ([n  (in-range num)])
    (+ start (* n step))))

(define (linear-seq start end num #:start? [start? #t] #:end? [end? #t])
  (cond
    [(zero? num)  empty]
    ; ambiguous request: arbitrarily return start
    [(and start? end? (= 1 num))  (list start)]
    [else
     (define size (- end start))
     (define step (/ size (cond [(and start? end?)  (- num 1)]
                                [(or start? end?)   (- num 1/2)]
                                [else               num])))
     (define real-start
       (cond [start?  start]
             [else    (+ start (* 1/2 step))]))
     
     (build-linear-seq real-start step num)]))

#;
(begin
  (require rackunit)
  (check-equal? (linear-seq 0 1 2 #:start? #t #:end? #t) '(0 1))
  (check-equal? (linear-seq 0 1 2 #:start? #t #:end? #f) '(0 2/3))
  (check-equal? (linear-seq 0 1 2 #:start? #f #:end? #t) '(1/3 1))
  (check-equal? (linear-seq 0 1 2 #:start? #f #:end? #f) '(1/4 3/4)))

(define (linear-seq* points num #:start? [start? #t] #:end? [end? #t])
  (let/ec return
    (when (empty? points) (raise-type-error 'linear-seq* "nonempty (listof real?)" points))
    
    (define pts (list->vector points))
    (define len (vector-length pts))
    
    (define indexes (linear-seq 0 (sub1 len) num #:start? start? #:end? end?))
    (define int-parts (map floor indexes))
    (define frac-parts (map - indexes int-parts))
    (map (λ (i f)
           (if (= i (sub1 len))
               (vector-ref pts i)
               (alpha-blend (vector-ref pts (add1 i)) (vector-ref pts i) f)))
         int-parts frac-parts)))

(define (nan? x) (eqv? x +nan.0))
(define (infinite? x) (or (eqv? x +inf.0) (eqv? x -inf.0)))
(define (regular? x) (and (real? x) (not (nan? x)) (not (infinite? x))))

(define (min2 x y)
  (cond [(x . < . y)  x]
        [(y . < . x)  y]
        [(exact? x)  x]
        [else  y]))

(define (max2 x y)
  (cond [(x . > . y)  x]
        [(y . > . x)  y]
        [(exact? x)  x]
        [else  y]))

(define (clamp x mn mx) (min (max x mn) mx))
(define (clamp* x mn mx) (min2 (max2 x mn) mx))

(define (min* x . xs) (foldl min2 x xs))
(define (max* x . xs) (foldl max2 x xs))

(define (maybe-min x . xs)
  (for/fold ([x x]) ([y  (in-list xs)])
    (if x (if y (min* x y) x)
        (if y y #f))))

(define (maybe-max x . xs)
  (for/fold ([x x]) ([y  (in-list xs)])
    (if x (if y (max* x y) x)
        (if y y #f))))

(define 180/pi (fl/ 180.0 pi))
(define pi/180 (fl/ pi 180.0))

(define (degrees->radians d)
  (fl* (exact->inexact d) pi/180))

(define (radians->degrees r)
  (fl* (exact->inexact r) 180/pi))

(define (floor-log10 x)
  (inexact->exact (floor (/ (log (abs x)) (log 10)))))

(define (ceiling-log10 x)
  (inexact->exact (ceiling (/ (log (abs x)) (log 10)))))

;; Like real->decimal-string, but removes trailing zeros
(define (real->string/trunc x e)
  (define str (real->decimal-string x e))
  (let loop ([x  (string-length str)])
    (cond [(zero? x)  "0"]
          [(char=? #\0 (string-ref str (sub1 x)))  (loop (sub1 x))]
          [(char=? #\. (string-ref str (sub1 x)))  (substring str 0 (sub1 x))]
          [else  (substring str 0 x)])))

(define (digits-for-range x-min x-max [extra-digits 3])
  (define range (- x-max x-min))
  (+ extra-digits (if (zero? range) 0 (max 0 (- (floor-log10 range))))))

(define ((flnewton-invert f f-diff f-inv-guess n) y)
  (let ([y  (exact->inexact y)])
    (let loop ([x  (f-inv-guess y)] [n n])
      (let/ec return
        (when (zero? n) (return x))
        
        (define dx (fl/ (fl- y (f x)) (f-diff x)))
        (when (zero? dx) (return x))
        
        (loop (fl- x dx) (sub1 n))))))
