#lang racket/base

(require racket/list racket/match
         "../common/math.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; 3D plot renderers

(struct renderer3d (render-fun ticks-fun bounds-fun x-min x-max y-min y-max z-min z-max)
  #:transparent)

(define (make-renderer3d render-fun ticks-fun bounds-fun x-min x-max y-min y-max z-min z-max)
  (when (and x-min x-max (x-min . > . x-max))
    (error 'make-renderer3d "expected x-min <= x-max; got x-min = ~e and x-max = ~e" x-min x-max))
  (when (and y-min y-max (y-min . > . y-max))
    (error 'make-renderer3d "expected y-min <= y-max; got y-min = ~e and y-max = ~e" y-min y-max))
  (when (and z-min z-max (z-min . > . z-max))
    (error 'make-renderer3d "expected z-min <= z-max; got z-min = ~e and z-max = ~e" z-min z-max))
  (renderer3d render-fun ticks-fun bounds-fun x-min x-max y-min y-max z-min z-max))

(define (null-3d-render-fun area) empty)
(define (null-3d-ticks-fun . _) (values empty empty empty))
(define null-3d-bounds-fun values)

(define null-renderer3d
  (renderer3d null-3d-render-fun null-3d-ticks-fun null-3d-bounds-fun #f #f #f #f #f #f))

;; ===================================================================================================
;; Bounds application; fixpoint computation for multiple renderers

(define (apply-bounds renderer x-min x-max y-min y-max z-min z-max)
  (match-define (renderer3d _1 _2 bounds-fun rx-min rx-max ry-min ry-max rz-min rz-max) renderer)
  (bounds-fun (maybe-max x-min rx-min) (maybe-min x-max rx-max)
              (maybe-max y-min ry-min) (maybe-min y-max ry-max)
              (maybe-max z-min rz-min) (maybe-min z-max rz-max)))

(define (apply-bounds* rs x-min x-max y-min y-max z-min z-max)
  (define-values (x-mins x-maxs y-mins y-maxs z-mins z-maxs)
    (for/lists (x-mins x-maxs y-mins y-maxs z-mins z-maxs) ([renderer  (in-list rs)])
      (apply-bounds renderer  x-min x-max y-min y-max z-min z-max)))
  (values (maybe-max x-min (apply maybe-min x-mins)) (maybe-min x-max (apply maybe-max x-maxs))
          (maybe-max y-min (apply maybe-min y-mins)) (maybe-min y-max (apply maybe-max y-maxs))
          (maybe-max z-min (apply maybe-min z-mins)) (maybe-min z-max (apply maybe-max z-maxs))))

(define (bounds-fixpoint rs x-min x-max y-min y-max z-min z-max [max-iters 4])
  (let/ec break
    (for/fold ([x-min x-min] [x-max x-max] [y-min y-min] [y-max y-max] [z-min z-min] [z-max z-max]
                             ) ([n  (in-range max-iters)])
      (define-values (nx-min nx-max ny-min ny-max nz-min nz-max)
        (apply-bounds* rs x-min x-max y-min y-max z-min z-max))
      (cond [(and (equal? nx-min x-min) (equal? nx-max x-max)
                  (equal? ny-min y-min) (equal? ny-max y-max)
                  (equal? nz-min z-min) (equal? nz-max z-max))
             (break x-min x-max y-min y-max z-min z-max)]
            [else  (values nx-min nx-max ny-min ny-max nz-min nz-max)]))))
