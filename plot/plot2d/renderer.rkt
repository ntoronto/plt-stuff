#lang racket/base

(require racket/list racket/match
         "../common/math.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; 2D plot renderers

(struct renderer2d (render-proc ticks-fun bounds-fun x-min x-max y-min y-max)
  #:transparent)

(define (make-renderer2d render-proc ticks-fun bounds-fun x-min x-max y-min y-max)
  (when (and x-min x-max (x-min . > . x-max))
    (error 'make-renderer2d "expected x-min <= x-max; got x-min = ~e and x-max = ~e" x-min x-max))
  (when (and y-min y-max (y-min . > . y-max))
    (error 'make-renderer2d "expected y-min <= y-max; got y-min = ~e and y-max = ~e" y-min y-max))
  (renderer2d render-proc ticks-fun bounds-fun x-min x-max y-min y-max))

(define (null-2d-render-proc area) empty)
(define (null-2d-ticks-fun . _) (values empty empty))
(define null-2d-bounds-fun values)

(define null-renderer2d
  (renderer2d null-2d-render-proc null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

;; ===================================================================================================
;; Bounds application; fixpoint computation for multiple renderers

(define (apply-bounds renderer x-min x-max y-min y-max)
  (match-define (renderer2d _1 _2 bounds-fun rx-min rx-max ry-min ry-max) renderer)
  (bounds-fun (maybe-max x-min rx-min) (maybe-min x-max rx-max)
              (maybe-max y-min ry-min) (maybe-min y-max ry-max)))

(define (apply-bounds* rs x-min x-max y-min y-max)
  (define-values (x-mins x-maxs y-mins y-maxs)
    (for/lists (x-mins x-maxs y-mins y-maxs) ([renderer  (in-list rs)])
      (apply-bounds renderer x-min x-max y-min y-max)))
  (values (maybe-max x-min (apply maybe-min x-mins)) (maybe-min x-max (apply maybe-max x-maxs))
          (maybe-max y-min (apply maybe-min y-mins)) (maybe-min y-max (apply maybe-max y-maxs))))

(define (renderer2d-bounds-fixpoint rs x-min x-max y-min y-max [max-iters 4])
  (let/ec break
    (for/fold ([x-min x-min] [x-max x-max] [y-min y-min] [y-max y-max]) ([n  (in-range max-iters)])
      (define-values (nx-min nx-max ny-min ny-max)
        (apply-bounds* rs x-min x-max y-min y-max))
      (cond [(and (equal? nx-min x-min) (equal? nx-max x-max)
                  (equal? ny-min y-min) (equal? ny-max y-max))
             (break x-min x-max y-min y-max)]
            [else
             (values nx-min nx-max ny-min ny-max)]))))
