#lang racket/base

(require racket/gui/base racket/match racket/list racket/class
         (for-syntax racket/base)
         "../common/gui.rkt"
         "../common/math.rkt"
         "area.rkt"
         "snip.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define plot3d-width (make-parameter 400))
(define plot3d-height (make-parameter 400))

(define plot3d-new-window? (make-parameter #f))

(define plot3d-jpeg-quality (make-parameter 90))
(define plot3d-ps-interactive? (make-parameter #f))
(define plot3d-pdf-interactive? (make-parameter #f))

;; Every plot3d function has the same keyword arguments; this creates one
;; with (arbitrary) other arguments and unhygienically introduces identifiers
;; for the keyword arguments. It also provides call/kws to call other plot3d
;; functions, passing the keyword argument values.

(define-syntax (define/plot3d-kws stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (with-syntax
         ([(width height angle altitude title x-label y-label z-label
                  x-min x-max y-min y-max z-min z-max)
           (datum->syntax
            #'name '(width height angle altitude title x-label y-label z-label
                           x-min x-max y-min y-max z-min z-max))]
          [call/kws  (datum->syntax #'name 'call/plot3d-kws)])
       (syntax/loc stx
         (define (name args ...
                       #:width [width (plot3d-width)]
                       #:height [height (plot3d-height)]
                       #:angle [angle (plot3d-angle)]
                       #:altitude [altitude (plot3d-altitude)]
                       #:title [title (plot3d-title)]
                       #:x-label [x-label (plot3d-x-label)]
                       #:y-label [y-label (plot3d-y-label)]
                       #:z-label [z-label (plot3d-z-label)]
                       #:x-min [x-min #f] #:x-max [x-max #f]
                       #:y-min [y-min #f] #:y-max [y-max #f]
                       #:z-min [z-min #f] #:z-max [z-max #f])
           (define (call/kws name
                             #:width [width width]
                             #:height [height height]
                             #:angle [angle angle]
                             #:altitude [altitude altitude]
                             #:title [title title]
                             #:x-label [x-label x-label]
                             #:y-label [y-label y-label]
                             #:z-label [z-label z-label]
                             #:x-min [x-min x-min] #:x-max [x-max x-max]
                             #:y-min [y-min y-min] #:y-max [y-max y-max]
                             #:z-min [z-min z-min] #:z-max [z-max z-max]
                             . ap-args)
             (apply name ap-args
                    #:width width #:height height
                    #:angle angle #:altitude altitude
                    #:title title #:x-label x-label
                    #:y-label y-label #:z-label z-label
                    #:x-min x-min #:x-max x-max
                    #:y-min y-min #:y-max y-max
                    #:z-min z-min #:z-max z-max))
           (let () body ...))))]))

(define (apply-bounds-funs rs x-min x-max y-min y-max z-min z-max)
  (define-values (x-mins x-maxs y-mins y-maxs z-mins z-maxs)
    (for/lists (x-mins x-maxs y-mins y-maxs z-mins z-maxs)
               ([renderer  (in-list rs)])
      (match-define (renderer3d f x-ticks y-ticks z-ticks bounds-fun
                                rx-min rx-max ry-min ry-max rz-min rz-max)
        renderer)
      (bounds-fun (maybe-max x-min rx-min) (maybe-min x-max rx-max)
                  (maybe-max y-min ry-min) (maybe-min y-max ry-max)
                  (maybe-max z-min rz-min) (maybe-min z-max rz-max))))
  (values (maybe-max x-min (apply maybe-min x-mins))
          (maybe-min x-max (apply maybe-max x-maxs))
          (maybe-max y-min (apply maybe-min y-mins))
          (maybe-min y-max (apply maybe-max y-maxs))
          (maybe-max z-min (apply maybe-min z-mins))
          (maybe-min z-max (apply maybe-max z-maxs))))

;; render to a given dc

(define/plot3d-kws (plot3d/dc rend-or-rends dc)
  (define rs (cond [(list? rend-or-rends)  rend-or-rends]
                   [else  (list rend-or-rends)]))
  
  (define-values (px-min px-max py-min py-max pz-min pz-max)
    (for/fold ([px-min x-min] [px-max x-max]
               [py-min y-min] [py-max y-max]
               [pz-min z-min] [pz-max z-max])
              ([n  (in-range 4)])
      (apply-bounds-funs rs px-min px-max py-min py-max pz-min pz-max)))
  
  (let ([x-min  (if x-min x-min px-min)]
        [x-max  (if x-max x-max px-max)]
        [y-min  (if y-min y-min py-min)]
        [y-max  (if y-max y-max py-max)]
        [z-min  (if z-min z-min pz-min)]
        [z-max  (if z-max z-max pz-max)])
    (when (or (not x-min) (not x-max) (x-min . >= . x-max))
      (error 'plot3d
             "could not determine x bounds; got: x-min = ~e, x-max = ~e"
             x-min x-max))
    (when (or (not y-min) (not y-max) (y-min . >= . y-max))
      (error 'plot3d
             "could not determine y bounds; got: y-min = ~e, y-max = ~e"
             y-min y-max))
    (when (or (not z-min) (not z-max) (z-min . >= . z-max))
      (error 'plot3d
             "could not determine z bounds; got: z-min = ~e, z-max = ~e"
             z-min z-max))
    
    (define x-ticks
      (remove-duplicates
       (append* (map (λ (r) ((renderer3d-x-ticks r) x-min x-max)) rs))))
    
    (define y-ticks
      (remove-duplicates
       (append* (map (λ (r) ((renderer3d-y-ticks r) y-min y-max)) rs))))
    
    (define z-ticks
      (remove-duplicates
       (append* (map (λ (r) ((renderer3d-z-ticks r) z-min z-max)) rs))))
    
    (parameterize ([plot3d-angle     angle]
                   [plot3d-altitude  altitude]
                   [plot3d-title     title]
                   [plot3d-x-label   x-label]
                   [plot3d-y-label   y-label]
                   [plot3d-z-label   z-label])
      (define area
        (make-object 3d-plot-area%
          x-ticks y-ticks z-ticks x-min x-max y-min y-max z-min z-max dc))
      (send area start-plot)
      (for ([renderer  (in-list rs)])
        (match-define (renderer3d f x-ticks y-ticks z-ticks bounds-fun
                                  rx-min rx-max ry-min ry-max rz-min rz-max)
          renderer)
        (send area clip-to-bounds rx-min rx-max ry-min ry-max rz-min rz-max)
        (f area))
      (send area end-plot))))

;; render to a bitmap

(define/plot3d-kws (plot3d->bitmap renderer)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (call/plot3d-kws plot3d/dc renderer dc)
  bm)

;; render to a snip

(define/plot3d-kws (plot3d->snip renderer)
  (make-object 3d-plot-snip%
    (λ (angle altitude anim?)
      (parameterize ([plot3d-animating?  (if anim? #t (plot3d-animating?))])
        (call/plot3d-kws plot3d->bitmap renderer
                         #:angle angle #:altitude altitude)))
    angle altitude))

;; render to a frame

(define/plot3d-kws (plot3d->frame renderer)
  (make-snip-frame
   (call/plot3d-kws plot3d->snip renderer)
   width height (if title (format "Plot: ~a" title) "Plot")))

;; render to a frame or a snip, depending on (plot3d-new-window?)

(define/plot3d-kws (plot3d renderer)
  (cond [(plot3d-new-window?)
         (define frame (call/plot3d-kws plot3d->frame renderer))
         (send frame show #t)
         (void)]
        [else  (call/plot3d-kws plot3d->snip renderer)]))

;; render to a bitmap file

(define/plot3d-kws (plot3d->bitmap-file renderer output kind)
  (send (call/plot3d-kws plot3d->bitmap renderer)
        save-file output kind (plot3d-jpeg-quality)))

;; render to a vector graphics file (ps, pdf, svg)

(define/plot3d-kws (plot3d->vector-file renderer output kind)
  (define dc
    (case kind
      [(ps)  (new post-script-dc%
                  [interactive (plot3d-ps-interactive?)]
                  [parent #f] [use-paper-bbox #f] [as-eps #t]
                  [width width] [height height] [output output])]
      [(pdf)  (new pdf-dc%
                   [interactive (plot3d-pdf-interactive?)]
                   [parent #f] [use-paper-bbox #f]
                   [width width] [height height] [output output])]
      [(svg)  (new svg-dc%
                   [width width] [height height] [output output]
                   [exists 'truncate/replace])]))
  (send dc start-doc "Rendering plot")
  (send dc start-page)
  (call/plot3d-kws plot3d/dc renderer dc)
  (send dc end-page)
  (send dc end-doc))

;; render to any supported kind of file

(define/plot3d-kws (plot3d->file renderer output kind)
  (case kind
    [(png jpeg xbm xpm bmp)
     (call/plot3d-kws plot3d->bitmap-file renderer output kind)]
    [(ps pdf svg)
     (call/plot3d-kws plot3d->vector-file renderer output kind)])
  (void))
