#lang scribble/manual

@(require scribble/eval
          "common.rkt")

@title[#:tag "plot2d"]{2D Plotting and Renderers}

@defmodule["../plot2d.rkt"]

@section[#:tag "plot2d:plot"]{Plotting}

@plot2d:doc{
Plots a 2D renderer or list of renderers, as returned by @(racket points), @(racket function), @(racket contour), @(racket histogram), and others.

By default, @(racket plot2d) produces a Racket value that is displayed as an image and can be manipulated like any other value.
For example, they may be put in lists:

@interaction[#:eval plot-eval
(list (plot2d (function sin (- pi) pi)
              #:width 150 #:height 150
              #:x-label #f #:y-label #f)
      (plot2d (function sqr -2 2)
              #:width 150 #:height 150
              #:x-label #f #:y-label #f))]

When the parameter @(racket plot2d-new-window?) is @(racket #t), @(racket plot2d) opens a new window to display the plot and returns @(racket (void)).

When given, the @(racket x-min), @(racket x-max), @(racket y-min) and @(racket y-max) arguments determine the bounds of the plot, but not the bounds of the renderers. For example,

@interaction[#:eval plot-eval
(plot2d (function (λ (x) (sin (* 4 x))) -1 1)
        #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5)]

Here, the renderer draws in [-1,1] × [-1,1], but the plot area is [-1.5,1.5] × [-1.5,1.5].

Plots are created in four steps:
@itemlist[
#:style 'ordered
        @item{Work with renderers to determine the bounds of the plot.}
        @item{Combine the renderers' @italic{x}- and @italic{y}-axis ticks and tick labels.}
        @item{Create a plot area with the determined bounds; draw axes, title, and other surrounding elements.}
        @item{For each renderer:
              @itemlist[
              #:style 'ordered
                      @item{Limit drawing to the renderer's bounds.}
                      @item{Invoke the renderer, passing the plot area.}]}]
}

@deftogether[
(@defproc[(plot2d->bitmap [renderer (or/c renderer2d? (listof renderer2d?))] ...)
          (is-a?/c bitmap%)]
 @defproc[(plot2d->snip [renderer (or/c renderer2d? (listof renderer2d?))] ...)
         (is-a?/c snip%)]
 @defproc[(plot2d->frame [renderer (or/c renderer2d? (listof renderer2d?))] ...)
          (is-a?/c frame%)]
 @defproc[(plot2d->file [renderer (or/c renderer2d? (listof renderer2d?))]
                        [output (or/c path-string? output-port?)]
                        [kind
                         (one-of/c 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg)]
                        ...)
          void?])]{
Plot to different backends. Each of these functions has the same keyword arguments as @(racket plot2d).

Use @(racket plot2d->bitmap) to create a bitmap. This is mostly useful in slideshows; for example:

@racketblock[
(require slideshow plot2d)

(plot2d-font-size (* 2 (plot2d-font-size)))
(plot2d-pen-width (* 2 (plot2d-pen-width)))

(slide
 #:title "A 2D Parabola"
 (item "This is a 2D parabola:")
 (bitmap
  (plot2d->bitmap (function sqr -1 1)
                  #:width 500 #:height 500)))]

Use @(racket plot2d->frame) when you need control over the attributes of a window containing a plot; for example, to keep it initially hidden.

Use @(racket plot2d->file) to save a plot to a file.
When creating a JPEG file, the parameter @(racket plot2d-jpeg-quality) determines its quality.
When creating a PostScript or PDF file, the parameters @(racket plot2d-ps-interactive?) and @(racket plot2d-pdf-interactive?) determine whether the user is given a dialog for setting printing parameters (see @(racket post-script-dc%) and @(racket pdf-dc%)).
}

@plot2d/dc:doc{
Plots to an arbitrary device context. The width and height of the plot are the device context's width and height.

Use this if you need to continually update a plot on a @(racket canvas%), or to create other @(racket plot2d)-like functions with different backends.
}

@section[#:tag "plot2d:points"]{Point Renderers}

@points:doc{
Creates a renderer that renders points. Use it, for example, to draw 2D scatter plots.

The renderer returned by @(racket points) sets its bounds to the smallest rectangle that contains the points.
Still, it is often necessary to override these bounds, especially with randomized data. For example,

@interaction[#:eval plot-eval
(let ([xs  (build-list 20 (λ _ (random)))]
      [ys  (build-list 20 (λ _ (random)))])
  (list (plot2d (points (map vector xs ys))
                #:width 150 #:height 150)
        (plot2d (points (map vector xs ys)
                        #:x-min 0 #:x-max 1
                        #:y-min 0 #:y-max 1)
                #:width 150 #:height 150)))]

Readers of the first plot could only guess that the random points were generated in [0,1] × [0,1].

The @(racket #:label) argument may be any Unicode string or a symbol in @(racket known-point-symbols).
}

@defthing[known-point-symbols (listof symbol?)]{
A list containing the symbols that are valid @(racket points) labels.

@interaction[#:eval plot-eval known-point-symbols]
}

@vector-field:doc{
Creates a renderer that draws a vector field.

@examples[#:eval plot-eval
(plot2d (vector-field (λ (x y) (vector (+ x y) (- x y)))
                      -2 2 -2 2 #:arrow-length 'scaled))
(plot2d (vector-field (λ (x y) (vector (+ x y) (- x y)))
                      -2 2 -2 2 #:arrow-length 'normalized))
(plot2d (vector-field (λ (x y) (vector (+ x y) (- x y)))
                      -2 2 -2 2 #:arrow-length 'real))]
}

@section[#:tag "plot2d:line"]{Line Renderers}

@lines:doc{
}

@parametric:doc{
}

@function:doc{
}

@inverse:doc{
Like @(racket function), but maps @italic{y} values to @italic{x} values.

@interaction[#:eval plot-eval
(plot2d (list (function sqr)
              (function identity #:color "black" #:style 'dot)
              (inverse sqr #:color "darkblue"))
        #:x-min -2 #:x-max 4 #:y-min -2 #:y-max 4)]
}
