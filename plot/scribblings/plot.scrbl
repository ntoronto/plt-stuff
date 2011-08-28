#lang scribble/manual

@(require "common.rkt")

@title[#:tag "top"]{@(plot-name): Extensible Graph Plotting}
@author{@(author+email "Neil Toronto" "neil.toronto@gmail.com")}

@(plot-name) provides an extensible interface for producing nearly any kind of plot.
It includes many common kinds already, such as scatter plots, line plots, contour plots, and 3D surface plots.
Thanks to Racket's excellent multiple-backend drawing library, @(plot-name) can render plots as manipulatable images in @(racket DrRacket), as bitmaps in slideshows, as PNG, PDF, PS and SVG files, or on any device context.

@table-of-contents[]

@include-section["quick.scrbl"]

@include-section["plot2d.scrbl"]

@include-section["plot3d.scrbl"]

@include-section["params.scrbl"]

@include-section["custom.scrbl"]
