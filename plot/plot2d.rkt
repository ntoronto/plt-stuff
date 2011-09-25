#lang racket/base

(require "plot2d/sample.rkt")
(provide
 ; parameters
 plot2d-x-transform plot2d-x-transform:doc
 plot2d-y-transform plot2d-y-transform:doc)


(require "plot2d/area.rkt")
(provide
 ; parameters
 plot2d-tick-size plot2d-tick-size:doc
 plot2d-title plot2d-title:doc
 plot2d-x-label plot2d-x-label:doc
 plot2d-y-label plot2d-y-label:doc)


(require "plot2d/ticks.rkt")
(provide
 ; parameters
  plot2d-tick-skip plot2d-tick-skip:doc)


(require "plot2d/plot.rkt")
(provide
 ; functions
 plot2d/dc plot2d/dc:doc
 plot2d->bitmap plot2d->bitmap:doc
 plot2d->snip plot2d->snip:doc
 plot2d->frame plot2d->frame:doc
 plot2d plot2d:doc
 plot2d->file plot2d->file:doc)


(require "plot2d/point.rkt")
(provide
 ; parameters
 point-symbol point-symbol:doc
 point-color point-color:doc
 point-size point-size:doc
 point-line-width point-line-width:doc
 point-alpha point-alpha:doc
 vector-field-samples vector-field-samples:doc
 vector-field-color vector-field-color:doc
 vector-field-line-width vector-field-line-width:doc
 vector-field-line-style vector-field-line-style:doc
 vector-field-arrow-scale vector-field-arrow-scale:doc
 vector-field-alpha vector-field-alpha:doc
 error-bar-width error-bar-width:doc
 error-bar-line-color error-bar-line-color:doc
 error-bar-line-width error-bar-line-width:doc
 error-bar-line-style error-bar-line-style:doc
 error-bar-alpha error-bar-alpha:doc
 ; renderers
 points points:doc
 vector-field vector-field:doc
 error-bars error-bars:doc)


(require "plot2d/line.rkt")
(provide
 ; parameters
 line-samples line-samples:doc
 line-color line-color:doc
 line-width line-width:doc
 line-style line-style:doc
 line-alpha line-alpha:doc
 ; renderers
 lines lines:doc
 parametric parametric:doc
 polar polar:doc
 function function:doc
 inverse inverse:doc)


(require "plot2d/interval.rkt")
(provide
 ; parameters
 interval-samples interval-samples:doc
 interval-color interval-color:doc
 interval-style interval-style:doc
 interval-alpha interval-alpha:doc
 ; renderers
 lines-interval lines-interval:doc
 parametric-interval parametric-interval:doc
 polar-interval polar-interval:doc
 function-interval function-interval:doc
 inverse-interval inverse-interval:doc)


(require "plot2d/contour.rkt")
(provide
 default-contour-line-colors default-contour-line-colors:doc
 default-contour-fill-colors default-contour-fill-colors:doc
 ; parameters
 contour-levels contour-levels:doc
 contour-samples contour-samples:doc
 contour-colors contour-colors:doc
 contour-widths contour-widths:doc
 contour-styles contour-styles:doc
 contour-alphas contour-alphas:doc
 contour-interval-colors contour-interval-colors:doc
 contour-interval-alphas contour-interval-alphas:doc
 ; renderers
 contours contours:doc
 contour-intervals contour-intervals:doc)


(require "plot2d/histogram.rkt")
(provide
 ; parameters
 histogram-color histogram-color:doc
 histogram-style histogram-style:doc
 histogram-line-color histogram-line-color:doc
 histogram-line-width histogram-line-width:doc
 histogram-line-style histogram-line-style:doc
 histogram-alpha histogram-alpha:doc
 ; renderers
 histogram histogram:doc)


(require "plot2d/decoration.rkt")
(provide
 ; parameters
 x-axis-ticks? x-axis-ticks?:doc
 y-axis-ticks? y-axis-ticks?:doc
 polar-axes-number polar-axes-number:doc
 polar-axes-ticks? polar-axes-ticks?:doc
 label-anchor label-anchor:doc
 label-angle label-angle:doc
 label-alpha label-alpha:doc
 ; axis and tick renderers
 x-axis x-axis:doc
 y-axis y-axis:doc
 axes axes:doc
 polar-axes polar-axes:doc
 x-tick-lines x-tick-lines:doc
 y-tick-lines y-tick-lines:doc
 tick-grid tick-grid:doc
 ; label renderers
 point-label point-label:doc
 parametric-label parametric-label:doc
 polar-label polar-label:doc
 function-label function-label:doc
 inverse-label inverse-label:doc)
