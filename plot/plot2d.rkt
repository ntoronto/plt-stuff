#lang racket/base

(require racket/contract racket/gui
         "plot2d/area.rkt"
         "plot2d/renderer.rkt"
         "plot2d/plot.rkt"
         "plot2d/line.rkt"
         "plot2d/points.rkt"
         "plot2d/contour.rkt"
         "plot2d/histogram.rkt"
         "common/contract.rkt")

(provide (all-from-out
          "plot2d/line.rkt"
          "plot2d/points.rkt"
          "plot2d/contour.rkt"))

;; area.rkt
(provide/contract
 [plot2d-tick-skip    (parameter/c positive-integer/c)]
 [plot2d-tick-size    (parameter/c nonnegative-real/c)]
 [plot2d-title        (parameter/c (or/c string? #f))]
 [plot2d-x-label      (parameter/c string?)]
 [plot2d-y-label      (parameter/c string?)]
 [plot2d-fg-color     (parameter/c plot-color/c)]
 [plot2d-bg-color     (parameter/c plot-color/c)]
 [plot2d-font-size    (parameter/c positive-integer/c)]
 [plot2d-font-family  (parameter/c font-family/c)]
 [plot2d-pen-width    (parameter/c nonnegative-real/c)])

;; renderer.rkt
(provide/contract
 [mix  (->* (renderer2d?) #:rest (listof renderer2d?) renderer2d?)])

;; plot.rkt
(provide/contract
 [plot2d-width   (parameter/c positive-integer/c)]
 [plot2d-height  (parameter/c positive-integer/c)]
 [plot2d-x-min   (parameter/c real?)]
 [plot2d-x-max   (parameter/c real?)]
 [plot2d-y-min   (parameter/c real?)]
 [plot2d-y-max   (parameter/c real?)]
 [plot2d-jpeg-quality      (parameter/c (integer-in 0 100))]
 [plot2d-ps-interactive?   (parameter/c boolean?)]
 [plot2d-pdf-interactive?  (parameter/c boolean?)]
 [plot2d->bitmap
  (->* (renderer2d?)
       (#:width positive-integer/c #:height positive-integer/c
        #:x-min real? #:x-max real? #:y-min real? #:y-max real?
        #:title string? #:x-label string? #:y-label string?)
       (is-a?/c bitmap%))]
 [plot2d
  (->* (renderer2d?)
       (#:width positive-integer/c #:height positive-integer/c
        #:x-min real? #:x-max real? #:y-min real? #:y-max real?
        #:title string? #:x-label string? #:y-label string?)
       (is-a?/c image-snip%))]
 [plot2d->file
  (->* (renderer2d? (or/c path-string? output-port?)
                    (one-of/c 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg))
       (#:width positive-integer/c #:height positive-integer/c
        #:x-min real? #:x-max real? #:y-min real? #:y-max real?
        #:title string? #:x-label string? #:y-label string?)
       void)])

;; histogram.rkt
(provide/contract
 [histogram-bar-color   (parameter/c plot-color/c)]
 [histogram-bar-style   (parameter/c brush-style/c)]
 [histogram-line-color  (parameter/c plot-color/c)]
 [histogram-line-width  (parameter/c nonnegative-real/c)]
 [histogram-line-style  (parameter/c pen-style/c)]
 [histogram-alpha       (parameter/c (real-in 0 1))]
 [histogram  (->* ((listof (cons/c any/c real?)))
                  ((or/c real? #f) (or/c real? #f) #:x-min real? #:x-max real?
                   #:bar-color plot-color/c #:bar-style brush-style/c
                   #:line-color plot-color/c #:line-width nonnegative-real/c
                   #:line-style pen-style/c #:alpha (real-in 0 1))
                  renderer2d?)])
