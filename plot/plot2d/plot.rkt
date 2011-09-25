#lang racket/base

(require racket/gui/base racket/contract racket/list racket/class racket/match
         (for-syntax racket/base)
         "../common/gui.rkt"
         "../common/math.rkt"
         "../common/contract.rkt"
         "../common/legend.rkt"
         "../common/file-type.rkt"
         "../common/area.rkt"
         "../common/plot.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Plot to a given dc

(defproc (plot2d/dc [renderer  (or/c renderer2d? (listof renderer2d?))]
                    [dc  (is-a?/c dc<%>)]
                    [#:x-min x-min (or/c real? #f) #f]
                    [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f]
                    [#:y-max y-max (or/c real? #f) #f]
                    [#:title title (or/c string? #f) (plot2d-title)]
                    [#:x-label x-label (or/c string? #f) (plot2d-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot2d-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                    ) void?
  (define rs (flatten (list renderer)))
  
  (define-values (px-min px-max py-min py-max)
    (renderer2d-bounds-fixpoint rs x-min x-max y-min y-max))
  
  (let ([x-min  (if x-min x-min px-min)]
        [x-max  (if x-max x-max px-max)]
        [y-min  (if y-min y-min py-min)]
        [y-max  (if y-max y-max py-max)])
    (when (or (not x-min) (not x-max) (x-min . >= . x-max))
      (error 'plot2d
             "could not determine nonempty x axis; got: x-min = ~e, x-max = ~e"
             x-min x-max))
    (when (or (not y-min) (not y-max) (y-min . >= . y-max))
      (error 'plot2d
             "could not determine nonempty y axis; got: y-min = ~e, y-max = ~e"
             y-min y-max))
    
    (let ([x-min  (inexact->exact x-min)]
          [x-max  (inexact->exact x-max)]
          [y-min  (inexact->exact y-min)]
          [y-max  (inexact->exact y-max)])
      (define-values (all-x-ticks all-y-ticks)
        (for/lists (all-x-ticks all-y-ticks) ([r  (in-list rs)])
          ((renderer2d-ticks-fun r) x-min x-max y-min y-max)))
      
      (define x-ticks (remove-duplicates (append* all-x-ticks)))
      (define y-ticks (remove-duplicates (append* all-y-ticks)))
      
      (parameterize ([plot2d-title        title]
                     [plot2d-x-label      x-label]
                     [plot2d-y-label      y-label]
                     [plot-legend-anchor  legend-anchor])
        (define area (make-object 2d-plot-area%
                       x-ticks y-ticks x-min x-max y-min y-max dc))
        (send area start-plot)
        
        (define legend-entries
          (flatten (for/list ([renderer  (in-list rs)])
                     (match-define (renderer2d render-proc ticks-fun bounds-fun
                                               rx-min rx-max ry-min ry-max)
                       renderer)
                     (send area reset-drawing-params)
                     (send area clip-to-bounds rx-min rx-max ry-min ry-max)
                     (render-proc area))))
        
        (send area end-plot)
        
        (when (not (empty? legend-entries))
          (send area put-legend legend-entries))))))

;; Every further plot2d function has the same keyword arguments. This macro creates one with
;; (arbitrary) other arguments and unhygienically introduces identifiers for the keyword arguments.
;; It also provides call/plot2d-kws to call other plot2d functions, passing the keyword argument
;; values.
(define-syntax (defproc/plot2d-kws stx)
  (syntax-case stx ()
    [(_ (name args ...) return/c body ...)
     (with-syntax
         ([(call/kws x-min x-max y-min y-max width height
                     title x-label y-label legend-anchor)
           (datum->syntax #'name '(call/plot2d-kws x-min x-max y-min y-max width height
                                                   title x-label y-label legend-anchor))])
       (syntax/loc stx
         (defproc (name args ...
                        [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                        [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                        [#:width width (integer>=/c 1) (plot-width)]
                        [#:height height (integer>=/c 1) (plot-height)]
                        [#:title title (or/c string? #f) (plot2d-title)]
                        [#:x-label x-label (or/c string? #f) (plot2d-x-label)]
                        [#:y-label y-label (or/c string? #f) (plot2d-y-label)]
                        [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                        ) return/c
           (define (call/kws name
                             #:x-min [x-min x-min] #:x-max [x-max x-max]
                             #:y-min [y-min y-min] #:y-max [y-max y-max]
                             #:width [width width] #:height [height height] #:title [title title]
                             #:x-label [x-label x-label] #:y-label [y-label y-label]
                             #:legend-anchor [legend-anchor legend-anchor]
                             . ap-args)
             (apply name ap-args
                    #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                    #:width width #:height height #:title title #:x-label x-label #:y-label y-label
                    #:legend-anchor legend-anchor))
           (let () body ...))))]))

;; ===================================================================================================
;; Plot to various other backends

;; Plot to a bitmap
(defproc/plot2d-kws (plot2d->bitmap [renderer (or/c renderer2d? (listof renderer2d?))]
                                    ) (is-a?/c bitmap%)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (plot2d/dc renderer dc
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
             #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)
  bm)

;; Plot to a snip
(defproc/plot2d-kws (plot2d->snip [renderer (or/c renderer2d? (listof renderer2d?))]) (is-a?/c snip%)
  (make-object image-snip% (call/plot2d-kws plot2d->bitmap renderer)))

;; Plot to a frame
(defproc/plot2d-kws (plot2d->frame [renderer (or/c renderer2d? (listof renderer2d?))]
                                   ) (is-a?/c frame%)
  (make-snip-frame (call/plot2d-kws plot2d->snip renderer)
                   width height
                   (if title (format "Plot: ~a" title) "Plot")))

;; Plot to a frame or a snip, depending on (plot2d-new-window?)
(defproc/plot2d-kws (plot2d [renderer (or/c renderer2d? (listof renderer2d?))]
                            ) (or/c (is-a?/c snip%) void?)
  (cond [(plot-new-window?)  (define frame (call/plot2d-kws plot2d->frame renderer))
                             (send frame show #t)
                             (void)]
        [else  (call/plot2d-kws plot2d->snip renderer)]))

;; Plot to a bitmap file
(defproc/plot2d-kws (plot2d->bitmap-file [renderer (or/c renderer2d? (listof renderer2d?))]
                                         [output (or/c path-string? output-port?)]
                                         [kind (one-of/c 'png 'jpeg 'xmb 'xpm 'bmp)]) void?
  (define bm (call/plot2d-kws plot2d->bitmap renderer))
  (send bm save-file output kind (plot-jpeg-quality))
  (void))

;; Plot to a vector graphics file (ps, pdf, svg)
(defproc/plot2d-kws (plot2d->vector-file [renderer (or/c renderer2d? (listof renderer2d?))]
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
  (plot2d/dc renderer dc
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
             #:title title #:x-label x-label #:y-label y-label
             #:legend-anchor legend-anchor)
  (send dc end-page)
  (send dc end-doc)
  (void))

;; render to any supported kind of file

(defproc/plot2d-kws
  (plot2d->file [renderer (or/c renderer2d? (listof renderer2d?))]
                [output (or/c path-string? output-port?)]
                [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]) void?
  (case kind
    [(auto)                  (call/plot2d-kws plot2d->file renderer output
                                              (detect-image-file-type output))]
    [(png jpeg xbm xpm bmp)  (call/plot2d-kws plot2d->bitmap-file renderer output kind)]
    [(ps pdf svg)            (call/plot2d-kws plot2d->vector-file renderer output kind)]))
