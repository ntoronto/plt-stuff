#lang racket/base

(require racket/contract
         "common/contract.rkt"
         (only-in "plot2d/renderer.rkt" renderer2d?))


(require "plot2d/area.rkt")

(provide
 plot2d-tick-skip plot2d-tick-skip:doc
 plot2d-tick-size plot2d-tick-size:doc
 plot2d-title plot2d-title:doc
 plot2d-x-label plot2d-x-label:doc
 plot2d-y-label plot2d-y-label:doc
 plot2d-foreground plot2d-foreground:doc
 plot2d-background plot2d-background:doc
 plot2d-font-size plot2d-font-size:doc
 plot2d-font-family plot2d-font-family:doc
 plot2d-pen-width plot2d-pen-width:doc)


(require "plot2d/plot.rkt")

(provide 
 ; parameters
 plot2d-width plot2d-width:doc
 plot2d-height plot2d-height:doc
 plot2d-new-window? plot2d-new-window?:doc
 plot2d-jpeg-quality plot2d-jpeg-quality:doc
 plot2d-ps-interactive? plot2d-ps-interactive?:doc
 plot2d-pdf-interactive? plot2d-pdf-interactive?:doc
 ; functions
 plot2d/dc plot2d/dc:doc
 plot2d->bitmap plot2d->bitmap:doc
 plot2d->snip plot2d->snip:doc
 plot2d->frame plot2d->frame:doc
 plot2d plot2d:doc
 plot2d->file plot2d->file:doc)

(require "plot2d/points.rkt"
         (only-in "common/points.rkt" known-point-symbols))

(provide
 ; parameters
 point-label point-label:doc
 point-color point-color:doc
 point-size point-size:doc
 point-line-width point-line-width:doc
 point-alpha point-alpha:doc
 vector-field-samples vector-field-samples:doc
 vector-field-color vector-field-color:doc
 vector-field-line-width vector-field-line-width:doc
 vector-field-line-style vector-field-line-style:doc
 vector-field-arrow-length vector-field-arrow-length:doc
 vector-field-alpha vector-field-alpha:doc
 ; functions
 points points:doc
 vector-field vector-field:doc
 error-bars error-bars:doc
 known-point-symbols)


(require "plot2d/line.rkt")

(provide
 ; parameters
 line-samples line-samples:doc
 line-color line-color:doc
 line-width line-width:doc
 line-style line-style:doc
 line-alpha line-alpha:doc
 ; functions
 lines lines:doc
 parametric parametric:doc
 function function:doc
 inverse inverse:doc)


(require "plot2d/contour.rkt")

(provide
 contour-levels contour-levels:doc
 contour-samples contour-samples:doc
 contour-color contour-color:doc
 contour-width contour-width:doc
 contour-style contour-style:doc
 contour-alpha contour-alpha:doc
 shade-color-function shade-color-function:doc)

(provide/contract
 [contour
  (->* ((real? real? . -> . real?))
       ((or/c real? #f) (or/c real? #f) (or/c real? #f) (or/c real? #f)
        #:samples positive-integer/c
        #:color plot-color/c #:width nonnegative-real/c
        #:style pen-style/c #:alpha (real-in 0 1))
       renderer2d?)]
 [shade
  (->* ((real? real? . -> . real?))
       ((or/c real? #f) (or/c real? #f) (or/c real? #f) (or/c real? #f)
        #:samples positive-integer/c
        #:colors (real? real? positive-integer/c . -> . plot-color/c))
       renderer2d?)])


(require "plot2d/histogram.rkt")

(provide/contract
 [histogram-bar-color   (parameter/c plot-color/c)]
 [histogram-bar-style   (parameter/c brush-style/c)]
 [histogram-line-color  (parameter/c plot-color/c)]
 [histogram-line-width  (parameter/c nonnegative-real/c)]
 [histogram-line-style  (parameter/c pen-style/c)]
 [histogram-alpha       (parameter/c (real-in 0 1))]
 [histogram
  (->* ((listof (vector/c any/c real?)))
       ((or/c real? #f) (or/c real? #f) #:x-min real? #:x-max (or/c real? #f)
        #:bar-color plot-color/c #:bar-style brush-style/c
        #:line-color plot-color/c #:line-width nonnegative-real/c
        #:line-style pen-style/c #:alpha (real-in 0 1))
       renderer2d?)])
