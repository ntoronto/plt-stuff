#lang racket/base

(require racket/gui racket/match
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define plot2d-width (make-parameter 400))
(define plot2d-height (make-parameter 400))

(define plot2d-x-min (make-parameter -5))
(define plot2d-x-max (make-parameter 5))
(define plot2d-y-min (make-parameter -5))
(define plot2d-y-max (make-parameter 5))

(define plot2d-jpeg-quality (make-parameter 90))
(define plot2d-ps-interactive (make-parameter #f))
(define plot2d-pdf-interactive (make-parameter #f))

(define (plot2d->bitmap renderer
                        #:width [width (plot2d-width)]
                        #:height [height (plot2d-height)]
                        #:x-min [x-min #f] #:x-max [x-max #f]
                        #:y-min [y-min #f] #:y-max [y-max #f]
                        #:title [title (plot2d-title)]
                        #:x-label [x-label (plot2d-x-label)]
                        #:y-label [y-label (plot2d-y-label)])
  (match-define (renderer2d f x-ticks y-ticks rx-min rx-max ry-min ry-max)
    renderer)
  (let ([x-min  (if x-min x-min (if rx-min rx-min (plot2d-x-min)))]
        [x-max  (if x-max x-max (if rx-max rx-max (plot2d-x-max)))]
        [y-min  (if y-min y-min (if ry-min ry-min (plot2d-y-min)))]
        [y-max  (if y-max y-max (if ry-max ry-max (plot2d-y-max)))])
    (define bm (make-bitmap width height))
    (define dc (make-object bitmap-dc% bm))
    (parameterize ([plot2d-title    title]
                   [plot2d-x-label  x-label]
                   [plot2d-y-label  y-label])
      (define area (make-object 2d-plot-area%
                     x-ticks y-ticks x-min x-max y-min y-max dc))
      (send area start-plot)
      (f area)
      (send area end-plot))
    bm))

;; plot2d : renderer2d -> image-snip%
(define (plot2d renderer
                #:width [width (plot2d-width)]
                #:height [height (plot2d-height)]
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f]
                #:title [title (plot2d-title)]
                #:x-label [x-label (plot2d-x-label)]
                #:y-label [y-label (plot2d-y-label)])
  (make-object image-snip% 
    (plot2d->bitmap renderer
                    #:width width #:height height
                    #:x-min x-min #:x-max x-max
                    #:y-min y-min #:y-max y-max
                    #:title title #:x-label x-label #:y-label y-label)))

(define (plot2d->bitmap-file renderer output kind
                             #:width [width (plot2d-width)]
                             #:height [height (plot2d-height)]
                             #:x-min [x-min #f] #:x-max [x-max #f]
                             #:y-min [y-min #f] #:y-max [y-max #f]
                             #:title [title (plot2d-title)]
                             #:x-label [x-label (plot2d-x-label)]
                             #:y-label [y-label (plot2d-y-label)])
  (send (plot2d->bitmap renderer
                        #:width width #:height height
                        #:x-min x-min #:x-max x-max
                        #:y-min y-min #:y-max y-max
                        #:title title #:x-label x-label #:y-label y-label)
        save-file output kind (plot2d-jpeg-quality)))

(define (plot2d->vector-file
         renderer output kind
         #:width [width (plot2d-width)]
         #:height [height (plot2d-height)]
         #:x-min [x-min #f] #:x-max [x-max #f]
         #:y-min [y-min #f] #:y-max [y-max #f]
         #:title [title (plot2d-title)]
         #:x-label [x-label (plot2d-x-label)]
         #:y-label [y-label (plot2d-y-label)])
  (match-define (renderer2d f x-ticks y-ticks rx-min rx-max ry-min ry-max)
    renderer)
  (let ([x-min  (if x-min x-min (if rx-min rx-min (plot2d-x-min)))]
        [x-max  (if x-max x-max (if rx-max rx-max (plot2d-x-max)))]
        [y-min  (if y-min y-min (if ry-min ry-min (plot2d-y-min)))]
        [y-max  (if y-max y-max (if ry-max ry-max (plot2d-y-max)))])
    (define dc
      (case kind
        [(ps)  (new post-script-dc%
                    [interactive (plot2d-ps-interactive)]
                    [parent #f] [use-paper-bbox #f] [as-eps #t]
                    [width width] [height height] [output output])]
        [(pdf)  (new pdf-dc%
                     [interactive (plot2d-pdf-interactive)]
                     [parent #f] [use-paper-bbox #f]
                     [width width] [height height] [output output])]
        [(svg)  (new svg-dc%
                     [width width] [height height] [output output] 
                     [exists 'truncate/replace])]
        [else  (raise-type-error 'plot2d->vector-file "one of (ps pdf svg)"
                                 kind)]))
    (send dc start-doc "Rendering plot")
    (send dc start-page)
    (parameterize ([plot2d-title    title]
                   [plot2d-x-label  x-label]
                   [plot2d-y-label  y-label])
      (define area (make-object 2d-plot-area%
                     x-ticks y-ticks x-min x-max y-min y-max dc))
      (send area start-plot)
      (f area)
      (send area end-plot))
    (send dc end-page)
    (send dc end-doc)))

(define (plot2d->file
         renderer output kind
         #:width [width (plot2d-width)]
         #:height [height (plot2d-height)]
         #:x-min [x-min #f] #:x-max [x-max #f]
         #:y-min [y-min #f] #:y-max [y-max #f]
         #:title [title (plot2d-title)]
         #:x-label [x-label (plot2d-x-label)]
         #:y-label [y-label (plot2d-y-label)])
  (case kind
    [(png jpeg xbm xpm bmp)
     (plot2d->bitmap-file renderer output kind
                          #:width width #:height height
                          #:x-min x-min #:x-max x-max
                          #:y-min y-min #:y-max y-max
                          #:title title #:x-label x-label #:y-label y-label)]
    [(ps pdf svg)
     (plot2d->vector-file renderer output kind
                          #:width width #:height height
                          #:x-min x-min #:x-max x-max
                          #:y-min y-min #:y-max y-max
                          #:title title #:x-label x-label #:y-label y-label)]
    [else  (raise-type-error 'plot2d->file
                             "one of (png jpeg xmb xpm bmp ps pdf svg)"
                             kind)]))
