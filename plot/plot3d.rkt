#lang racket/base

(require racket/contract racket/gui/base racket/class
         "common/contract.rkt"
         (only-in "plot3d/renderer.rkt" renderer3d?))


(require "plot3d/area.rkt")

(provide/contract
 [plot3d-samples          (parameter/c positive-integer/c)]
 [plot3d-angle            (parameter/c real?)]
 [plot3d-altitude         (parameter/c real?)]
 [plot3d-ambient-light    (parameter/c (real-in 0 1))]
 [plot3d-diffuse-light?   (parameter/c boolean?)]
 [plot3d-specular-light?  (parameter/c boolean?)]
 [plot3d-tick-skip        (parameter/c positive-integer/c)]
 [plot3d-tick-size        (parameter/c nonnegative-real/c)]
 [plot3d-title            (parameter/c (or/c string? #f))]
 [plot3d-x-label          (parameter/c (or/c string? #f))]
 [plot3d-y-label          (parameter/c (or/c string? #f))]
 [plot3d-z-label          (parameter/c (or/c string? #f))]
 [plot3d-foreground       (parameter/c plot-color/c)]
 [plot3d-background       (parameter/c plot-color/c)]
 [plot3d-font-size        (parameter/c positive-integer/c)]
 [plot3d-font-family      (parameter/c font-family/c)]
 [plot3d-pen-width        (parameter/c nonnegative-real/c)])


(require "plot3d/plot.rkt")

(provide/contract
 [plot3d-width             (parameter/c positive-integer/c)]
 [plot3d-height            (parameter/c positive-integer/c)]
 [plot3d-new-window?       (parameter/c boolean?)]
 [plot3d-jpeg-quality      (parameter/c (integer-in 0 100))]
 [plot3d-ps-interactive?   (parameter/c boolean?)]
 [plot3d-pdf-interactive?  (parameter/c boolean?)]
 [plot3d/dc       (->/plot3d-kws (or/c renderer3d? (listof renderer3d?))
                                 (is-a?/c dc<%>)
                                 void)]
 [plot3d->bitmap  (->/plot3d-kws (or/c renderer3d? (listof renderer3d?))
                                 (is-a?/c bitmap%))]
 [plot3d->snip    (->/plot3d-kws (or/c renderer3d? (listof renderer3d?))
                                 (is-a?/c snip%))]
 [plot3d->frame   (->/plot3d-kws (or/c renderer3d? (listof renderer3d?))
                                 (is-a?/c frame%))]
 [plot3d          (->/plot3d-kws (or/c renderer3d? (listof renderer3d?))
                                 (or/c (is-a?/c snip%) void))]
 [plot3d->file
  (->/plot3d-kws (or/c renderer3d? (listof renderer3d?))
                 (or/c path-string? output-port?)
                 (one-of/c 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg)
                 void)])


(require "plot3d/surface.rkt")

(provide/contract
 [surface3d-samples     (parameter/c positive-integer/c)]
 [surface3d-color       (parameter/c plot-color/c)]
 [surface3d-line-color  (parameter/c plot-color/c)]
 [surface3d-line-width  (parameter/c nonnegative-real/c)]
 [surface3d-line-style  (parameter/c pen-style/c)]
 [surface3d-alpha       (parameter/c (real-in 0 1))]
 [contour3d-levels      (parameter/c (or/c 'auto positive-integer/c))]
 [contour3d-line-color  (parameter/c plot-color/c)]
 [contour3d-line-width  (parameter/c nonnegative-real/c)]
 [contour3d-line-style  (parameter/c pen-style/c)]
 [contour3d-alpha       (parameter/c (real-in 0 1))]
 [shade3d-colors        (parameter/c color-function/c)]
 [shade3d-line-colors   (parameter/c color-function/c)]
 [shade3d-line-width    (parameter/c nonnegative-real/c)]
 [shade3d-line-style    (parameter/c pen-style/c)]
 [shade3d-alpha         (parameter/c (real-in 0 1))]
 [surface3d
  (->* ((real? real? . -> . real?))
       ((or/c real? #f) (or/c real? #f) (or/c real? #f) (or/c real? #f)
        #:z-min (or/c real? #f) #:z-max (or/c real? #f)
        #:samples positive-integer/c #:color plot-color/c
        #:line-color plot-color/c #:line-width nonnegative-real/c
        #:line-style pen-style/c #:alpha (real-in 0 1))
       renderer3d?)]
 [contour3d
  (->* ((real? real? . -> . real?))
       ((or/c real? #f) (or/c real? #f) (or/c real? #f) (or/c real? #f)
        #:z-min (or/c real? #f) #:z-max (or/c real? #f)
        #:samples positive-integer/c
        #:line-color plot-color/c #:line-width nonnegative-real/c
        #:line-style pen-style/c #:alpha (real-in 0 1))
       renderer3d?)]
 [shade3d
  (->* ((real? real? . -> . real?))
       ((or/c real? #f) (or/c real? #f) (or/c real? #f) (or/c real? #f)
        #:z-min (or/c real? #f) #:z-max (or/c real? #f)
        #:samples positive-integer/c #:colors color-function/c
        #:line-colors color-function/c #:line-width nonnegative-real/c
        #:line-style pen-style/c #:alpha (real-in 0 1))
       renderer3d?)])


(require "plot3d/line.rkt")

(provide/contract
 [line3d-samples  (parameter/c positive-integer/c)]
 [line3d-color  (parameter/c plot-color/c)]
 [line3d-width  (parameter/c nonnegative-real/c)]
 [line3d-style  (parameter/c pen-style/c)]
 [line3d-alpha  (parameter/c (real-in 0 1))]
 [lines3d
  (->* ((listof (vector/c real? real? real?)))
       (#:x-min (or/c real? #f) #:x-max (or/c real? #f)
        #:y-min (or/c real? #f) #:y-max (or/c real? #f)
        #:z-min (or/c real? #f) #:z-max (or/c real? #f)
        #:color plot-color/c #:width nonnegative-real/c #:style pen-style/c
        #:alpha (real-in 0 1))
       renderer3d?)]
 [parametric3d
  (->* ((or/c (real? . -> . (vector/c real? real? real?))
              (vector/c (real? . -> . real?)
                        (real? . -> . real?)
                        (real? . -> . real?)))
        real? real?)
       (#:x-min (or/c real? #f) #:x-max (or/c real? #f)
        #:y-min (or/c real? #f) #:y-max (or/c real? #f)
        #:z-min (or/c real? #f) #:z-max (or/c real? #f)
        #:samples positive-integer/c #:color plot-color/c
        #:width nonnegative-real/c #:style pen-style/c #:alpha (real-in 0 1))
       renderer3d?)])


(require "plot3d/points.rkt")

(provide/contract
 [point3d-label       (parameter/c point-label/c)]
 [point3d-color       (parameter/c plot-color/c)]
 [point3d-size        (parameter/c (integer-in 1 255))]
 [point3d-line-width  (parameter/c nonnegative-real/c)]
 [point3d-alpha       (parameter/c (real-in 0 1))]
 [points3d
  (->* ((listof (vector/c real? real? real?)))
       (#:x-min (or/c real? #f) #:x-max (or/c real? #f)
        #:y-min (or/c real? #f) #:y-max (or/c real? #f)
        #:z-min (or/c real? #f) #:z-max (or/c real? #f)
        #:label point-label/c #:color plot-color/c #:size (integer-in 1 255)
        #:line-width nonnegative-real/c #:alpha (real-in 0 1))
       renderer3d?)])
