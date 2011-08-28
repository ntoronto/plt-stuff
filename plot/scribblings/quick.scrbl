#lang scribble/manual

@(require scribble/eval
          "common.rkt")

@title[#:tag "quick"]{Quick Start}

@section[#:tag "quick:2d"]{2D Plots}

To plot a one-input, real-valued function, first @(racket (require plot2d)), and then try something like

@interaction[#:eval plot-eval
                    (plot2d (function sin (- pi) pi))]

Here, the first argument to @(racket function) is the function to be plotted, and @(racket (- pi)) and @(racket pi) define the closed interval, or @italic{bounds}, to plot it in.
The @(racket function) function constructs a @italic{renderer} that draws functions; @(racket plot2d) sets up a @italic{plot area} and passes it to the renderer.

@section[#:tag "quick:3d"]{3D Plots}

To plot a two-input, real-valued function as a surface, first @(racket (require plot3d)), and then try something like

@margin-note{The documentation can't show it, but in DrRacket you can rotate 3D plots by clicking on them and dragging the mouse. Try it!}

@interaction[#:eval plot-eval
                    (plot3d (surface3d (λ (x y) (* (cos x) (sin y)))
                                       (- pi) pi (- pi) pi)
                            #:title "An R × R → R function"
                            #:x-label "x" #:y-label "y" #:z-label "cos(x) sin(y)")]

This example also demonstrates using keyword arguments that change the plot, such as @(racket #:title).
In @(plot-name), every keyword argument is optional and almost all have parameterized default values.
In the case of @(racket plot3d)'s @(racket #:title), the corresponding parameter is @(racket plot3d-title).
That is, keyword arguments are usually shortcuts for parameterizing plots or renderers:

@margin-note{When parameterizing more than one plot, it is often easier to set parameters globally, as in @(racket (plot3d-title "Untitled")) and @(racket (plot3d-angle 45)).
                                                                              
There are many parameters that do not correspond to keyword arguments, such as @(racket plot2d-font-size); see (todo: ref plot-params) for the full listing.}

@interaction[#:eval plot-eval
                    (parameterize ([plot3d-title  "An R × R → R function"]
                                   [plot3d-x-label "x"]
                                   [plot3d-y-label "y"]
                                   [plot3d-z-label "cos(x) sin(y)"])
                      (plot3d (shade3d (λ (x y) (* (cos x) (sin y)))
                                       (- pi) pi (- pi) pi)))]

This example also demonstrates @(racket shade3d), which colors the surface between contour lines, or lines of constant height.
By default, @(racket shade3d) places the contour lines at the same heights as the ticks on the @italic{z} axis.

Renderers may be plotted together by passing them in a list:

@interaction[#:eval plot-eval
                    (plot2d (list (shade (λ (x y) (* (cos x) (sin y))))
                                  (contour (λ (x y) (* (cos x) (sin y)))))
                            #:x-min (- pi) #:x-max pi #:y-min (- pi) #:y-max pi
                            #:title "Shaded contour plot for cos(x) sin(y)")]

Here, @(racket shade) is the 2D version of @(racket shade3d), and @(racket contour) draws the contour lines that @(racket shade) colors between.
By default, both draw 10 contour levels (meaning 9 contour lines).

Because @(racket plot2d) asks renderers to draw in the plot area @italic{in order}, listing the contour lines first results in them being covered by contour shading:

@interaction[#:eval plot-eval
                    (plot2d (list (contour (λ (x y) (* (cos x) (sin y))))
                                  (shade (λ (x y) (* (cos x) (sin y)))))
                            #:x-min (- pi) #:x-max pi #:y-min (- pi) #:y-max pi
                            #:title "Shaded contour plot for cos(x) sin(y)")]

Notice that, in this example, @(racket contour) and @(racket shade) are not passed bounds.
The @italic{x} and @italic{y} minimum and maximum arguments to @(racket contour) and @(racket shade) are optional, as are many other bounds arguments.
When left out or @(racket #f), @(racket plot2d) or @(racket plot3d) works with the renderers to determine the bounds of the plot.
In this case, they settle on the bounds passed as keyword arguments to @(racket plot2d).

It is not always possible for the renderers and @(racket plot2d) or @(racket plot3d) to determine the bounds:

@interaction[#:eval plot-eval
                    (plot2d (function sin))
                    (plot2d (function sin #f #f))
                    (plot2d (function sin (- pi)))
                    (plot2d (list (function sin #f 0)
                                  (function sin 0 #f)))]

Unless overridden by keyword arguments like @(racket #:x-min), @(racket plot2d) and @(racket plot3d) construct plot areas with bounds that include every renderer:

@interaction[#:eval plot-eval
                    (plot2d (list (function (λ (x) (sqr (+ x 1))) -2 0
                                            #:color "green")
                                  (function (λ (x) (- (sqr (- x 1)))) 0 2
                                            #:color "blue")))]

@interaction[#:eval plot-eval
                    (plot3d (list (surface3d (λ (x y) (+ (sqr (+ x 1))
                                                         (sqr (+ y 1))))
                                             -2 0 -2 0 #:color "green")
                                  (shade3d (λ (x y) (+ (sqr (- x 1))
                                                       (sqr (- y 1))))
                                           0 2 0 2)))]

For an example of overriding bounds in a way that restricts a renderer, first consider

@interaction[#:eval plot-eval
                    (plot3d (parametric3d (λ (t)
                                            (vector (* (cos (* 40 t)) (cos t))
                                                    (* (sin (* 40 t)) (cos t))
                                                    (sin t)))
                                          (- pi) pi #:samples 3000)
                            #:altitude 30)]

This is an example of a 3D @italic{parametric} graph, which traces the path of a particle in 3D space from time @(racket t = t-min) to @(racket t = t-max) (in this case, from @(racket (- pi)) to @(racket pi)).
The @(racket #:samples) argument determines how many times to sample the particle's path.
This path is rather long---it traces out a sphere!---so we need more samples than the default.
The @(racket #:altitude) argument to @(racket plot3d) adjusts the viewing angle so that the sphere is not as ``squashed'' as it would be with the default altitude (@(racket 60)).

Passing bounds to @(racket plot3d) chops off the six axial poles:

@interaction[#:eval plot-eval
                    (plot3d (parametric3d (λ (t)
                                            (vector (* (cos (* 40 t)) (cos t))
                                                    (* (sin (* 40 t)) (cos t))
                                                    (sin t)))
                                          (- pi) pi #:samples 3000
                                          #:x-min -0.8 #:x-max 0.8
                                          #:y-min -0.8 #:y-max 0.8
                                          #:z-min -0.8 #:z-max 0.8)
                            #:altitude 30)]

Unlike with rendering 2D plots, rendering 3D plots is order-independent. (The shapes are sorted by their relative distance from the viewer and drawn back-to-front.) For example, here is a Green-Hornet-colored plane bisecting a Spider-Man-colored saddle:

@interaction[#:eval plot-eval
(let ([green-hornet-colors  (λ _ '("black" (0 128 0)))]
      [spider-man-colors (λ _ '((255 128 128) (128 128 255)))])
  (parameterize ([surface3d-samples  101])
    (plot3d (list (shade3d (λ (x y) (- (sqr x) (sqr y))) -2 2 -2 2
                           #:colors spider-man-colors
                           #:line-colors spider-man-colors)
                  (shade3d (λ (x y) (+ x y)) #:alpha 0.5
                           #:colors green-hornet-colors 
                           #:line-colors green-hornet-colors)
                  (contour3d (λ (x y) (- (sqr x) (sqr y)))
                             #:line-color "white")
                  (contour3d (λ (x y) (+ x y)) #:line-color "yellow")))))]

Notice that passing @(racket #:alpha 0.5) to @(racket shade3d) makes the Green Hornet's plane half-transparent.

All of these plots can be rendered to PNG, PDF, PS and SVG files using @(racket plot2d->file) and @(racket plot3d->file), to include in papers and other published media.

We suggest avoiding superhero colors in published work, however: stick to dark, fully saturated colors for lines (e.g. black, dark red, dark green) and very light, desaturated colors (e.g. white, very light pink, lilac) for surfaces.
These colors look more professional and print well in grayscale. Make exceptions when you need contrast; for example, in shading between contour lines.

@close-eval[plot-eval]
