#lang racket/base

(require "common/area.rkt")
(provide
 ; parameters
 plot-foreground plot-foreground:doc
 plot-background plot-background:doc
 plot-font-size plot-font-size:doc
 plot-font-family plot-font-family:doc
 plot-pen-width plot-pen-width:doc
 plot-legend-anchor plot-legend-anchor:doc
 plot-legend-box-alpha plot-legend-box-alpha:doc)


(require "common/plot.rkt")
(provide
 plot-width plot-width:doc
 plot-height plot-height:doc
 plot-new-window? plot-new-window?:doc
 plot-jpeg-quality plot-jpeg-quality:doc
 plot-ps-interactive? plot-ps-interactive?:doc
 plot-pdf-interactive? plot-pdf-interactive?:doc)


(require "common/point.rkt")
(provide known-point-symbols)
