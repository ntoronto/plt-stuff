#lang racket/base

(require "plot3d/sample.rkt")
(provide
 ; parameters
 plot3d-x-transform plot3d-x-transform:doc
 plot3d-y-transform plot3d-y-transform:doc
 plot3d-z-transform plot3d-z-transform:doc)


(require "plot3d/ticks.rkt")
(provide
 ; parameters
 plot3d-tick-skip plot3d-tick-skip:doc)


(require "plot3d/area.rkt")
(provide
 ; parameters
 plot3d-samples plot3d-samples:doc
 plot3d-animating? plot3d-animating?:doc
 plot3d-angle plot3d-angle:doc
 plot3d-altitude plot3d-altitude:doc
 plot3d-ambient-light-value plot3d-ambient-light-value:doc
 plot3d-diffuse-light? plot3d-diffuse-light?:doc
 plot3d-specular-light? plot3d-specular-light?:doc
 plot3d-tick-size plot3d-tick-size:doc
 plot3d-title plot3d-title:doc
 plot3d-x-label plot3d-x-label:doc
 plot3d-y-label plot3d-y-label:doc
 plot3d-z-label plot3d-z-label:doc
 ; functions
 samples/animating? samples/animating?:doc)


(require "plot3d/plot.rkt")
(provide
 ; functions
 plot3d/dc plot3d/dc:doc
 plot3d->bitmap plot3d->bitmap:doc
 plot3d->snip plot3d->snip:doc
 plot3d->frame plot3d->frame:doc
 plot3d plot3d:doc
 plot3d->file plot3d->file:doc)


(require "plot3d/surface.rkt")
(provide
 ; parameters
 surface3d-samples surface3d-samples:doc
 surface3d-color surface3d-color:doc
 surface3d-style surface3d-style:doc
 surface3d-line-color surface3d-line-color:doc
 surface3d-line-width surface3d-line-width:doc
 surface3d-line-style surface3d-line-style:doc
 surface3d-alpha surface3d-alpha:doc
 ; renderers
 surface3d surface3d:doc)


(require "plot3d/contour.rkt")
(provide
 default-contour3d-line-colors default-contour3d-line-colors:doc
 default-contour3d-fill-colors default-contour3d-fill-colors:doc
 ; parameters
 contour3d-samples contour3d-samples:doc
 contour3d-levels contour3d-levels:doc
 contour3d-colors contour3d-colors:doc
 contour3d-widths contour3d-widths:doc
 contour3d-styles contour3d-styles:doc
 contour3d-alphas contour3d-alphas:doc
 contour3d-interval-colors contour3d-interval-colors:doc
 contour3d-interval-line-colors contour3d-interval-line-colors:doc
 contour3d-interval-line-widths contour3d-interval-line-widths:doc
 contour3d-interval-line-styles contour3d-interval-line-styles:doc
 contour3d-interval-alphas contour3d-interval-alphas:doc
 ; renderers
 contours3d contours3d:doc
 contour3d-intervals contour3d-intervals:doc)


(require "plot3d/line.rkt")
(provide
 ; parameters
 line3d-samples line3d-samples:doc
 line3d-color line3d-color:doc
 line3d-width line3d-width:doc
 line3d-style line3d-style:doc
 line3d-alpha line3d-alpha:doc
 ; renderers
 lines3d lines3d:doc
 parametric3d parametric3d:doc)


(require "plot3d/point.rkt")
(provide
 ; parameters
 point3d-symbol point3d-symbol:doc
 point3d-color point3d-color:doc
 point3d-size point3d-size:doc
 point3d-line-width point3d-line-width:doc
 point3d-alpha point3d-alpha:doc
 ; renderers
 points3d points3d:doc)


(require "plot3d/isosurface.rkt")
(provide
 ; parameters
 isosurface3d-samples isosurface3d-samples:doc
 isosurface3d-color isosurface3d-color:doc
 isosurface3d-line-color isosurface3d-line-color:doc
 isosurface3d-line-width isosurface3d-line-width:doc
 isosurface3d-line-style isosurface3d-line-style:doc
 isosurface3d-alpha isosurface3d-alpha:doc
 isosurfaces3d-levels isosurfaces3d-levels:doc
 isosurfaces3d-colors isosurfaces3d-colors:doc
 isosurfaces3d-line-colors isosurfaces3d-line-colors:doc
 isosurfaces3d-line-widths isosurfaces3d-line-widths:doc
 isosurfaces3d-line-styles isosurfaces3d-line-styles:doc
 isosurfaces3d-alphas isosurfaces3d-alphas:doc
 polar3d-samples polar3d-samples:doc
 polar3d-color polar3d-color:doc
 polar3d-line-color polar3d-line-color:doc
 polar3d-line-width polar3d-line-width:doc
 polar3d-line-style polar3d-line-style:doc
 polar3d-alpha polar3d-alpha:doc
 ; renderers
 isosurface3d isosurface3d:doc
 isosurfaces3d isosurfaces3d:doc
 polar3d polar3d:doc)
