#lang racket/base

(require racket/gui racket/match
         "../common/math.rkt"
         "area.rkt"
         "renderer.rkt")

(provide (all-defined-out))

(define plot3d-width (make-parameter 400))
(define plot3d-height (make-parameter 400))

(define plot3d-x-min (make-parameter -5))
(define plot3d-x-max (make-parameter 5))
(define plot3d-y-min (make-parameter -5))
(define plot3d-y-max (make-parameter 5))
(define plot3d-z-min (make-parameter -5))
(define plot3d-z-max (make-parameter 5))

(define plot3d-jpeg-quality (make-parameter 90))
(define plot3d-ps-interactive (make-parameter #f))
(define plot3d-pdf-interactive (make-parameter #f))

(define (plot3d->bitmap
         renderer
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
  (match-define (renderer3d f x-ticks y-ticks z-ticks
                            rx-min rx-max ry-min ry-max rz-min rz-max)
    renderer)
  (let ([x-min  (if x-min x-min (if rx-min rx-min (plot3d-x-min)))]
        [x-max  (if x-max x-max (if rx-max rx-max (plot3d-x-max)))]
        [y-min  (if y-min y-min (if ry-min ry-min (plot3d-y-min)))]
        [y-max  (if y-max y-max (if ry-max ry-max (plot3d-y-max)))]
        [z-min  (if z-min z-min (if rz-min rz-min (plot3d-z-min)))]
        [z-max  (if z-max z-max (if rz-max rz-max (plot3d-z-max)))])
    (define bm (make-bitmap width height))
    (define dc (make-object bitmap-dc% bm))
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
      (f area)
      (send area end-plot))
    bm))

;; plot3d : renderer3d -> image-snip%
(define (plot3d
         renderer
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
  (make-object image-snip% 
    (plot3d->bitmap renderer
                    #:width width #:height height
                    #:angle angle #:altitude altitude
                    #:title title #:x-label x-label
                    #:y-label y-label #:z-label z-label
                    #:x-min x-min #:x-max x-max
                    #:y-min y-min #:y-max y-max
                    #:z-min z-min #:z-max z-max)))

(define (plot3d->bitmap-file
         renderer output kind
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
  (send (plot3d->bitmap renderer
                        #:width width #:height height
                        #:angle angle #:altitude altitude
                        #:title title #:x-label x-label
                        #:y-label y-label #:z-label z-label
                        #:x-min x-min #:x-max x-max
                        #:y-min y-min #:y-max y-max
                        #:z-min z-min #:z-max z-max)
        save-file output kind (plot3d-jpeg-quality)))

(define (plot3d->vector-file
         renderer output kind
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
  (match-define (renderer3d f x-ticks y-ticks z-ticks
                            rx-min rx-max ry-min ry-max rz-min rz-max)
    renderer)
  (let ([x-min  (if x-min x-min (if rx-min rx-min (plot3d-x-min)))]
        [x-max  (if x-max x-max (if rx-max rx-max (plot3d-x-max)))]
        [y-min  (if y-min y-min (if ry-min ry-min (plot3d-y-min)))]
        [y-max  (if y-max y-max (if ry-max ry-max (plot3d-y-max)))]
        [z-min  (if z-min z-min (if rz-min rz-min (plot3d-z-min)))]
        [z-max  (if z-max z-max (if rz-max rz-max (plot3d-z-max)))])
    (define dc
      (case kind
        [(ps)  (new post-script-dc%
                    [interactive (plot3d-ps-interactive)]
                    [parent #f] [use-paper-bbox #f] [as-eps #t]
                    [width width] [height height] [output output])]
        [(pdf)  (new pdf-dc%
                     [interactive (plot3d-pdf-interactive)]
                     [parent #f] [use-paper-bbox #f]
                     [width width] [height height] [output output])]
        [(svg)  (new svg-dc%
                     [width width] [height height] [output output] 
                     [exists 'truncate/replace])]
        [else  (raise-type-error 'plot2d->vector-file "one of (ps pdf svg)"
                                 kind)]))
    (send dc start-doc "Rendering plot")
    (send dc start-page)
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
      (f area)
      (send area end-plot))
    (send dc end-page)
    (send dc end-doc)))

(define (plot3d->file
         renderer output kind
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
  (case kind
    [(png jpeg xbm xpm bmp)
     (plot3d->bitmap-file renderer output kind
                          #:width width #:height height
                          #:angle angle #:altitude altitude
                          #:title title #:x-label x-label
                          #:y-label y-label #:z-label z-label
                          #:x-min x-min #:x-max x-max
                          #:y-min y-min #:y-max y-max
                          #:z-min z-min #:z-max z-max)]
    [(ps pdf svg)
     (plot3d->vector-file renderer output kind
                          #:width width #:height height
                          #:angle angle #:altitude altitude
                          #:title title #:x-label x-label
                          #:y-label y-label #:z-label z-label
                          #:x-min x-min #:x-max x-max
                          #:y-min y-min #:y-max y-max
                          #:z-min z-min #:z-max z-max)]
    [else  (raise-type-error 'plot3d->file
                             "one of (png jpeg xmb xpm bmp ps pdf svg)"
                             kind)]))
