#lang racket/base

(require racket/gui/base racket/match racket/list racket/class racket/contract
         (for-syntax racket/base)
         "../common/gui.rkt"
         "../common/math.rkt"
         "../common/file-type.rkt"
         "../common/area.rkt"
         "../common/plot.rkt"
         "../common/contract.rkt"
         "area.rkt"
         "snip.rkt"
         "renderer.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Plot to a given dc

(defproc (plot3d/dc [renderer  (or/c renderer3d? (listof renderer3d?))]
                    [dc  (is-a?/c dc<%>)]
                    [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                    [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
                    [#:angle angle real? (plot3d-angle)] [#:altitude altitude real? (plot3d-altitude)]
                    [#:title title (or/c string? #f) (plot3d-title)]
                    [#:x-label x-label (or/c string? #f) (plot3d-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot3d-y-label)]
                    [#:z-label z-label (or/c string? #f) (plot3d-z-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                    ) void?
  (define rs (flatten (list renderer)))
  
  (define-values (px-min px-max py-min py-max pz-min pz-max)
    (bounds-fixpoint rs x-min x-max y-min y-max z-min z-max))
  
  (let ([x-min  (if x-min x-min px-min)]
        [x-max  (if x-max x-max px-max)]
        [y-min  (if y-min y-min py-min)]
        [y-max  (if y-max y-max py-max)]
        [z-min  (if z-min z-min pz-min)]
        [z-max  (if z-max z-max pz-max)])
    (when (or (not x-min) (not x-max) (x-min . >= . x-max))
      (error 'plot3d "could not determine x bounds; got: x-min = ~e, x-max = ~e" x-min x-max))
    (when (or (not y-min) (not y-max) (y-min . >= . y-max))
      (error 'plot3d "could not determine y bounds; got: y-min = ~e, y-max = ~e" y-min y-max))
    (when (or (not z-min) (not z-max) (z-min . >= . z-max))
      (error 'plot3d "could not determine z bounds; got: z-min = ~e, z-max = ~e" z-min z-max))
    
    (let ([x-min  (inexact->exact x-min)]
          [x-max  (inexact->exact x-max)]
          [y-min  (inexact->exact y-min)]
          [y-max  (inexact->exact y-max)]
          [z-min  (inexact->exact z-min)]
          [z-max  (inexact->exact z-max)])
      (define-values (all-x-ticks all-y-ticks all-z-ticks)
        (for/lists (all-x-ticks all-y-ticks all-z-ticks) ([r  (in-list rs)])
          ((renderer3d-ticks-fun r) x-min x-max y-min y-max z-min z-max)))
      
      (define x-ticks (remove-duplicates (append* all-x-ticks)))
      (define y-ticks (remove-duplicates (append* all-y-ticks)))
      (define z-ticks (remove-duplicates (append* all-z-ticks)))
      
      (parameterize ([plot3d-angle        angle]
                     [plot3d-altitude     altitude]
                     [plot3d-title        title]
                     [plot3d-x-label      x-label]
                     [plot3d-y-label      y-label]
                     [plot3d-z-label      z-label]
                     [plot-legend-anchor  legend-anchor])
        (define area (make-object 3d-plot-area%
                       x-ticks y-ticks z-ticks x-min x-max y-min y-max z-min z-max dc))
        (send area start-plot)
        
        (define legend-entries
          (flatten (for/list ([renderer  (in-list rs)])
                     (match-define (renderer3d render-proc ticks-fun bounds-fun
                                               rx-min rx-max ry-min ry-max rz-min rz-max)
                       renderer)
                     (send area reset-drawing-params)
                     (send area clip-to-bounds rx-min rx-max ry-min ry-max rz-min rz-max)
                     (render-proc area))))
        
        (send area end-plot)
        
        (when (and (not (empty? legend-entries))
                   (or (not (plot3d-animating?))
                       (not (equal? (plot-legend-anchor) 'center))))
          (send area put-legend legend-entries))
        
        (when (plot3d-animating?) (send area put-angles))))))

;; Every further plot3d function has the same keyword arguments. This macro creates one with
;; (arbitrary) other arguments and unhygienically introduces identifiers for the keyword arguments.
;; It also provides call/plot3d-kws to call other plot3d functions, passing the keyword argument
;; values.
(define-syntax (defproc/plot3d-kws stx)
  (syntax-case stx ()
    [(_ (name args ...) return/c body ...)
     (with-syntax
         ([(call/kws x-min x-max y-min y-max z-min z-max width height angle altitude
                     title x-label y-label z-label legend-anchor)
           (datum->syntax #'name '(call/plot3d-kws
                                   x-min x-max y-min y-max z-min z-max width height angle altitude
                                   title x-label y-label z-label legend-anchor))])
       (syntax/loc stx
         (defproc (name args ...
                        [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                        [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                        [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
                        [#:width width (integer>=/c 1) (plot-width)]
                        [#:height height (integer>=/c 1) (plot-height)]
                        [#:angle angle real? (plot3d-angle)]
                        [#:altitude altitude real? (plot3d-altitude)]
                        [#:title title (or/c string? #f) (plot3d-title)]
                        [#:x-label x-label (or/c string? #f) (plot3d-x-label)]
                        [#:y-label y-label (or/c string? #f) (plot3d-y-label)]
                        [#:z-label z-label (or/c string? #f) (plot3d-z-label)]
                        [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                        ) return/c
           (define (call/kws name
                             #:x-min [x-min x-min] #:x-max [x-max x-max]
                             #:y-min [y-min y-min] #:y-max [y-max y-max]
                             #:z-min [z-min z-min] #:z-max [z-max z-max]
                             #:width [width width] #:height [height height]
                             #:angle [angle angle] #:altitude [altitude altitude]
                             #:title [title title] #:x-label [x-label x-label]
                             #:y-label [y-label y-label] #:z-label [z-label z-label]
                             #:legend-anchor [legend-anchor legend-anchor]
                             . ap-args)
             (apply
              name ap-args
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
              #:width width #:height height #:angle angle #:altitude altitude
              #:title title #:x-label x-label #:y-label y-label #:z-label z-label
              #:legend-anchor legend-anchor))
           (let () body ...))))]))

;; ===================================================================================================
;; Plot to various other backends

;; Plot to a bitmap
(defproc/plot3d-kws (plot3d->bitmap [renderer (or/c renderer3d? (listof renderer3d?))]
                                    ) (is-a?/c bitmap%)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (plot3d/dc renderer dc
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
             #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
             #:z-label z-label #:legend-anchor legend-anchor)
  bm)

;; Plot to a snip
(defproc/plot3d-kws (plot3d->snip [renderer (or/c renderer3d? (listof renderer3d?))]) (is-a?/c snip%)
  (make-object 3d-plot-snip%
    (λ (angle altitude anim?)
      (parameterize ([plot3d-animating?  (if anim? #t (plot3d-animating?))])
        (call/plot3d-kws plot3d->bitmap renderer #:angle angle #:altitude altitude)))
    angle altitude))

;; Plot to a frame
(defproc/plot3d-kws (plot3d->frame [renderer (or/c renderer3d? (listof renderer3d?))]
                                   ) (is-a?/c frame%)
  (make-snip-frame (call/plot3d-kws plot3d->snip renderer)
                   width height
                   (if title (format "Plot: ~a" title) "Plot")))

;; Plot to a frame or a snip, depending on the value of plot3d-new-window?
(defproc/plot3d-kws (plot3d [renderer (or/c renderer3d? (listof renderer3d?))]
                            ) (or/c (is-a?/c snip%) void?)
  (cond [(plot-new-window?)  (define frame (call/plot3d-kws plot3d->frame renderer))
                             (send frame show #t)
                             (void)]
        [else  (call/plot3d-kws plot3d->snip renderer)]))

;; Plot to a bitmap file
(defproc/plot3d-kws (plot3d->bitmap-file [renderer (or/c renderer3d? (listof renderer3d?))]
                                         [output (or/c path-string? output-port?)]
                                         [kind (one-of/c 'png 'jpeg 'xmb 'xpm 'bmp)]) void?
  (define bm (call/plot3d-kws plot3d->bitmap renderer))
  (send bm save-file output kind (plot-jpeg-quality))
  (void))

;; Plot to a vector graphics file (ps, pdf, svg)
(defproc/plot3d-kws (plot3d->vector-file [renderer (or/c renderer3d? (listof renderer3d?))]
                                         [output (or/c path-string? output-port?)]
                                         [kind (one-of/c 'ps 'pdf 'svg)]) void?
  (define dc
    (case kind
      [(ps)  (new post-script-dc%
                  [interactive (plot-ps-interactive?)] [parent #f] [use-paper-bbox #f] [as-eps #t]
                  [width width] [height height] [output output])]
      [(pdf)  (new pdf-dc%
                   [interactive (plot-pdf-interactive?)] [parent #f] [use-paper-bbox #f]
                   [width width] [height height] [output output])]
      [(svg)  (new svg-dc%
                   [width width] [height height] [output output] [exists 'truncate/replace])]))
  (send dc start-doc "Rendering plot")
  (send dc start-page)
  (plot3d/dc renderer dc
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
             #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
             #:z-label z-label #:legend-anchor legend-anchor)
  (send dc end-page)
  (send dc end-doc)
  (void))

;; Plot to any supported kind of file
(defproc/plot3d-kws
  (plot3d->file [renderer (or/c renderer3d? (listof renderer3d?))]
                [output (or/c path-string? output-port?)]
                [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]) void?
  (case kind
    [(auto)                  (call/plot3d-kws plot3d->file renderer output
                                              (detect-image-file-type output))]
    [(png jpeg xbm xpm bmp)  (call/plot3d-kws plot3d->bitmap-file renderer output kind)]
    [(ps pdf svg)            (call/plot3d-kws plot3d->vector-file renderer output kind)]))
