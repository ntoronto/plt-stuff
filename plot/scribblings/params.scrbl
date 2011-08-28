#lang scribble/manual

@(require scribble/eval
          "common.rkt")

@title[#:tag "plot-params"]{Plot and Renderer Parameters}

@section[#:tag "plot2d-params"]{2D Parameters}

@defmodule["../plot2d.rkt"]

@subsection[#:tag "plot2d-params:plot"]{Plot Parameters}

@(plot2d-width:doc)
@plot2d-height:doc{
The width and height of a plot, in logical drawing units (e.g. pixels for bitmap plots).
}

@plot2d-title:doc{}
@plot2d-x-label:doc{}
@plot2d-y-label:doc{}

@plot2d-new-window?:doc{
When @(racket #f), @(racket plot2d) returns an image snip. When @(racket #t), @(racket plot2d) returns @(racket (void)) and opens a new window containing the plot.

Command-line Racket cannot display image snips, but can open windows. Its users may want to set this to @(racket #t).
}

@plot2d-jpeg-quality:doc{
The quality of 
}

@plot2d-ps-interactive?:doc{}
@plot2d-pdf-interactive?:doc{}

@plot2d-tick-skip:doc{}
@plot2d-tick-size:doc{}
@plot2d-font-size:doc{}
@plot2d-font-family:doc{}
@plot2d-foreground:doc{}
@plot2d-background:doc{}
@plot2d-pen-width:doc{}

@subsection[#:tag "plot2d-params:point"]{Point Parameters}

@point-label:doc{}
@point-color:doc{}
@point-size:doc{}
@point-line-width:doc{}
@point-alpha:doc{}
@vector-field-samples:doc{}
@vector-field-color:doc{}
@vector-field-line-width:doc{}
@vector-field-arrow-length:doc{}
@vector-field-alpha:doc{}

@subsection[#:tag "plot2d-params:line"]{Line Parameters}

@line-samples:doc{}
@line-color:doc{}
@line-width:doc{}
@line-style:doc{}
@line-alpha:doc{}

@subsection[#:tag "plot2d-params:contour"]{Contour Parameters}

@contour-levels:doc{}
@contour-samples:doc{}
@contour-color:doc{}
@contour-width:doc{}
@contour-style:doc{}
@contour-alpha:doc{}
@shade-color-function:doc{}

@subsection[#:tag "plot2d-params:histogram"]{Histogram Parameters}

@section[#:tag "plot3d-params"]{3D Parameters}

@defmodule["../plot3d.rkt"]

@subsection[#:tag "plot3d-params:plot"]{Plot Parameters}

@subsection[#:tag "plot3d-params:point"]{Point Parameters}

@subsection[#:tag "plot3d-params:line"]{Line Parameters}

@subsection[#:tag "plot3d-params:surface"]{Surface Parameters}
