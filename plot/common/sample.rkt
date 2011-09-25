#lang racket/base

(require racket/match racket/flonum racket/math
         "math.rkt"
         "transform.rkt")

(provide (all-defined-out))

(define ((2d-polar->parametric f) θ)
  (let ([r  (exact->inexact (f θ))]
        [θ  (exact->inexact θ)])
  (vector (fl* r (flcos θ))
          (fl* r (flsin θ)))))

(define ((3d-polar->3d-function f) x y z)
  (let ([x  (exact->inexact x)]
        [y  (exact->inexact y)]
        [z  (exact->inexact z)])
    (define-values (θ ρ)
      (cond [(and (fl= x 0.0) (fl= y 0.0))  (values 0.0 0.0)]
            [else  (values (flmodulo (atan y x) 2pi)
                           (atan (fl/ z (fldist2 x y))))]))
    (define r (exact->inexact (f θ ρ)))
    (fl- r (fldist3 x y z))))

(define ((polar3d->parametric3d f num-orbits) t)
  (define θ (* num-orbits (* t pi)))
  (define ρ (asin t))
  (define r (f θ ρ))
  (vector (* r (cos θ) (cos ρ))
          (* r (sin θ) (cos ρ))
          (* r (sin ρ))))

(define (sample-parametric f t-min t-max samples)
  (map f (linear-seq t-min t-max samples)))

(define (sample-2d-polar f θ-min θ-max samples)
  (sample-parametric (2d-polar->parametric f) θ-min θ-max samples))

(define (sample-3d-polar f θ-min θ-max θ-samples ρ-min ρ-max ρ-samples)
  (for*/list ([θ  (in-list (linear-seq θ-min θ-max θ-samples))]
              [ρ  (in-list (linear-seq ρ-min ρ-max ρ-samples))])
    (let* ([r  (exact->inexact (f θ ρ))]
           [θ  (exact->inexact θ)]
           [ρ  (exact->inexact ρ)]
           [cos-ρ  (flcos ρ)])
      (vector (fl* r (fl* (flcos θ) cos-ρ))
              (fl* r (fl* (flsin θ) cos-ρ))
              (fl* r (flsin ρ))))))

(define ((make-function->sampler transform-thnk) f)
  (define memo (make-hash))
  (λ (x-min x-max x-samples)
    (define tx (transform-thnk))
    (hash-ref! memo (vector x-min x-max x-samples tx)
               (λ ()
                 (define xs (nonlinear-seq x-min x-max x-samples tx))
                 (list xs (map f xs))))))

(define ((make-2d-function->sampler transform-x-thnk transform-y-thnk) f)
  (define memo (make-hash))
  (λ (x-min x-max x-samples y-min y-max y-samples)
    (define tx (transform-x-thnk))
    (define ty (transform-y-thnk))
    (hash-ref! memo (vector x-min x-max x-samples tx y-min y-max y-samples ty)
               (λ ()
                 (define xs (nonlinear-seq x-min x-max x-samples tx))
                 (define ys (nonlinear-seq y-min y-max y-samples ty))
                 (list xs ys (for/vector #:length y-samples ([y  (in-list ys)])
                               (for/vector #:length x-samples ([x  (in-list xs)])
                                 (f x y))))))))

(define ((make-3d-function->sampler transform-x-thnk transform-y-thnk transform-z-thnk) f)
  (define memo (make-hash))
  (λ (x-min x-max x-samples y-min y-max y-samples z-min z-max z-samples)
    (define tx (transform-x-thnk))
    (define ty (transform-y-thnk))
    (define tz (transform-z-thnk))
    (hash-ref! memo (vector x-min x-max x-samples tx
                            y-min y-max y-samples ty
                            z-min z-max z-samples tz)
               (λ ()
                 (define xs (nonlinear-seq x-min x-max x-samples tx))
                 (define ys (nonlinear-seq y-min y-max y-samples ty))
                 (define zs (nonlinear-seq z-min z-max z-samples tz))
                 (list xs ys zs (for/vector #:length z-samples ([z  (in-list zs)])
                                  (for/vector #:length y-samples ([y  (in-list ys)])
                                    (for/vector #:length x-samples ([x  (in-list xs)])
                                      (f x y z)))))))))

(define (2d-sample->list zss)
  (for*/list ([zs  (in-vector zss)]
              [z   (in-vector zs)])
    z))

(define (3d-sample->list dsss)
  (for*/list ([dss  (in-vector dsss)]
              [ds   (in-vector dss)]
              [d    (in-vector ds)])
    d))
