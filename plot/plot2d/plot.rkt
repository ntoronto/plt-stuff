#lang racket/base

(require racket/gui/base racket/contract racket/list racket/class racket/match
         (for-syntax racket/base)
         "../common/gui.rkt"
         "../common/math.rkt"
         "../common/contract.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(defparam plot2d-width width positive-integer/c 400)
(defparam plot2d-height height positive-integer/c 400)

(defparam plot2d-new-window? new? boolean? #f)

(defparam plot2d-jpeg-quality quality (integer-in 0 100) 90)
(defparam plot2d-ps-interactive? interactive? boolean? #f)
(defparam plot2d-pdf-interactive? interactive? boolean? #f)

;; render to a given dc

(defproc (plot2d/dc [renderer  (or/c renderer2d? (listof renderer2d?))]
                    [dc  (is-a?/c dc<%>)]
                    [#:title title (or/c string? #f) (plot2d-title)]
                    [#:x-label x-label (or/c string? #f) (plot2d-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot2d-y-label)]
                    [#:x-min x-min (or/c real? #f) #f]
                    [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f]
                    [#:y-max y-max (or/c real? #f) #f]
                    ) void
  (define rs (cond [(list? renderer)  renderer]
                   [else  (list renderer)]))
  
  (define-values (px-min px-max py-min py-max)
    (for/fold ([px-min x-min] [px-max x-max] [py-min y-min] [py-max y-max])
      ([n  (in-range 4)])
      (apply-bounds-funs rs px-min px-max py-min py-max)))
  
  (let ([x-min  (if x-min x-min px-min)]
        [x-max  (if x-max x-max px-max)]
        [y-min  (if y-min y-min py-min)]
        [y-max  (if y-max y-max py-max)])
    (when (or (not x-min) (not x-max) (x-min . >= . x-max))
      (error 'plot2d
             "could not determine x bounds; got: x-min = ~e, x-max = ~e"
             x-min x-max))
    (when (or (not y-min) (not y-max) (y-min . >= . y-max))
      (error 'plot2d
             "could not determine y bounds; got: y-min = ~e, y-max = ~e"
             y-min y-max))
    
    (define x-ticks
      (remove-duplicates
       (append* (map (λ (r) ((renderer2d-x-ticks r) x-min x-max)) rs))))
    
    (define y-ticks
      (remove-duplicates
       (append* (map (λ (r) ((renderer2d-y-ticks r) y-min y-max)) rs))))
    
    (parameterize ([plot2d-title    title]
                   [plot2d-x-label  x-label]
                   [plot2d-y-label  y-label])
      (define area (make-object 2d-plot-area%
                     x-ticks y-ticks x-min x-max y-min y-max dc))
      (send area start-plot)
      (for ([renderer  (in-list rs)])
        (match-define (renderer2d f x-ticks y-ticks bounds-fun
                                  rx-min rx-max ry-min ry-max)
          renderer)
        (send area clip-to-bounds rx-min rx-max ry-min ry-max)
        (f area))
      (send area end-plot))))

#|
Every further plot2d function has the same keyword arguments; this creates one
with (arbitrary) other arguments and unhygienically introduces identifiers for
the keyword arguments. It also provides call/kws to call other plot2d functions,
passing the keyword argument values.
|#

(define-syntax (defproc/plot2d-kws stx)
  (syntax-case stx ()
    [(_ (name args ...) return/c body ...)
     (with-syntax
         ([(width height title x-label y-label x-min x-max y-min y-max)
           (datum->syntax
            #'name
            '(width height title x-label y-label x-min x-max y-min y-max))]
          [call/kws  (datum->syntax #'name 'call/plot2d-kws)])
       (syntax/loc stx
         (defproc (name args ...
                        [#:width width positive-integer/c (plot2d-width)]
                        [#:height height positive-integer/c (plot2d-height)]
                        [#:title title (or/c string? #f) (plot2d-title)]
                        [#:x-label x-label (or/c string? #f) (plot2d-x-label)]
                        [#:y-label y-label (or/c string? #f) (plot2d-y-label)]
                        [#:x-min x-min (or/c real? #f) #f]
                        [#:x-max x-max (or/c real? #f) #f]
                        [#:y-min y-min (or/c real? #f) #f]
                        [#:y-max y-max (or/c real? #f) #f])
           return/c
           (define (call/kws name
                             #:width [width width]
                             #:height [height height]
                             #:title [title title]
                             #:x-label [x-label x-label]
                             #:y-label [y-label y-label]
                             #:x-min [x-min x-min] #:x-max [x-max x-max]
                             #:y-min [y-min y-min] #:y-max [y-max y-max]
                             . ap-args)
             (apply name ap-args
                    #:width width #:height height
                    #:title title #:x-label x-label #:y-label y-label
                    #:x-min x-min #:x-max x-max
                    #:y-min y-min #:y-max y-max))
           (let () body ...))))]))

(define (apply-bounds-funs rs x-min x-max y-min y-max)
  (define-values (x-mins x-maxs y-mins y-maxs)
    (for/lists (x-mins x-maxs y-mins y-maxs) ([renderer  (in-list rs)])
      (match-define (renderer2d f x-ticks y-ticks bounds-fun
                                rx-min rx-max ry-min ry-max)
        renderer)
      (bounds-fun (maybe-max x-min rx-min) (maybe-min x-max rx-max)
                  (maybe-max y-min ry-min) (maybe-min y-max ry-max))))
  (values (maybe-max x-min (apply maybe-min x-mins))
          (maybe-min x-max (apply maybe-max x-maxs))
          (maybe-max y-min (apply maybe-min y-mins))
          (maybe-min y-max (apply maybe-max y-maxs))))

;; render to a bitmap

(defproc/plot2d-kws (plot2d->bitmap
                     [renderer  (or/c renderer2d? (listof renderer2d?))]
                     ) (is-a?/c bitmap%)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (plot2d/dc renderer dc
             #:title title #:x-label x-label #:y-label y-label
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max)
  bm)

;; render to a snip

(defproc/plot2d-kws (plot2d->snip
                     [renderer  (or/c renderer2d? (listof renderer2d?))]
                     ) (is-a?/c snip%)
  (make-object image-snip%
    (call/plot2d-kws plot2d->bitmap renderer)))

;; render to a frame

(defproc/plot2d-kws (plot2d->frame
                     [renderer  (or/c renderer2d? (listof renderer2d?))]
                     ) (is-a?/c frame%)
  (make-snip-frame
   (call/plot2d-kws plot2d->snip renderer)
   width height (if title (format "Plot: ~a" title) "Plot")))

;; render to a frame or a snip, depending on (plot2d-new-window?)

(defproc/plot2d-kws (plot2d
                     [renderer  (or/c renderer2d? (listof renderer2d?))]
                     ) (or/c (is-a?/c snip%) void)
  (cond [(plot2d-new-window?)
         (define frame (call/plot2d-kws plot2d->frame renderer))
         (send frame show #t)
         (void)]
        [else  (call/plot2d-kws plot2d->snip renderer)]))

;; render to a bitmap file

(defproc/plot2d-kws (plot2d->bitmap-file
                     [renderer  (or/c renderer2d? (listof renderer2d?))]
                     [output    (or/c path-string? output-port?)]
                     [kind      (one-of/c 'png 'jpeg 'xmb 'xpm 'bmp)]
                     ) void
  (send (call/plot2d-kws plot2d->bitmap renderer)
        save-file output kind (plot2d-jpeg-quality))
  (void))

;; render to a vector graphics file (ps, pdf, svg)

(defproc/plot2d-kws (plot2d->vector-file
                     [renderer (or/c renderer2d? (listof renderer2d?))]
                     [output (or/c path-string? output-port?)]
                     [kind (one-of/c 'ps 'pdf 'svg)]
                     ) void
  (define dc
    (case kind
      [(ps)  (new post-script-dc%
                  [interactive (plot2d-ps-interactive?)]
                  [parent #f] [use-paper-bbox #f] [as-eps #t]
                  [width width] [height height] [output output])]
      [(pdf)  (new pdf-dc%
                   [interactive (plot2d-pdf-interactive?)]
                   [parent #f] [use-paper-bbox #f]
                   [width width] [height height] [output output])]
      [(svg)  (new svg-dc%
                   [width width] [height height] [output output] 
                   [exists 'truncate/replace])]))
  (send dc start-doc "Rendering plot")
  (send dc start-page)
  (plot2d/dc renderer dc
             #:title title #:x-label x-label #:y-label y-label
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max)
  (send dc end-page)
  (send dc end-doc))

;; render to any supported kind of file

(defproc/plot2d-kws (plot2d->file
                     [renderer  (or/c renderer2d? (listof renderer2d?))]
                     [output  (or/c path-string? output-port?)]
                     [kind  (one-of/c 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg)]
                     ) void
  (case kind
    [(png jpeg xbm xpm bmp)
     (call/plot2d-kws plot2d->bitmap-file renderer output kind)]
    [(ps pdf svg)
     (call/plot2d-kws plot2d->vector-file renderer output kind)])
  (void))
