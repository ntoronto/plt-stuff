#lang racket/base

(require racket/match racket/draw racket/class racket/math
         "vector.rkt"
         "area.rkt")

(provide add-points known-point-symbols)

(define full-glyph-hash
  #hash((fullcircle . circle)
        (fullsquare . square)
        (fulldiamond . diamond)
        (fulltriangle . triangle)
        (fulltriangleup . triangleup)
        (fulltriangledown . triangledown)
        (fulltriangleleft . triangleleft)
        (fulltriangleright . triangleright)))

(define known-point-symbols
  '(dot point pixel plus times asterisk 5asterisk
        odot opoint opixel oplus otimes oasterisk o5asterisk
        circle square diamond triangle
        fullcircle fullsquare fulldiamond fulltriangle
        triangleup triangledown triangleleft triangleright
        fulltriangleup fulltriangledown fulltriangleleft fulltriangleright
        rightarrow leftarrow uparrow downarrow
        5star 6star full5star))

(define ((add-points vs label color size line-width alpha) area)
  (send area set-alpha alpha)
  (match label
    [(? string?)  (send area set-text-foreground color)
                  (send area set-font-size size)
                  (for ([v  (in-list vs)] #:when (vall-regular? v))
                    (send area add-text-glyph v label))]
    [(? symbol?)
     (send area set-pen color line-width 'solid)
     (define r (* 1/2 size))
     (define new-label
       (cond [(hash-has-key? full-glyph-hash label)
              (send area set-brush color 'solid)
              (hash-ref full-glyph-hash label)]
             [else
              (send area set-brush color 'transparent)
              label]))
     (for ([v  (in-list vs)] #:when (vall-regular? v))
       (case new-label
         ; circles
         [(circle)  (send area add-circle-glyph v r)]
         ; squares
         [(square)   (send area add-polygon-glyph v r 4 (* 1/4 pi))]
         [(diamond)  (send area add-polygon-glyph v r 4 0)]
         ; triangles
         [(triangle
           triangleup)     (send area add-polygon-glyph v r 3 (* -1/2 pi))]
         [(triangledown)   (send area add-polygon-glyph v r 3 (* 1/2 pi))]
         [(triangleleft)   (send area add-polygon-glyph v r 3 pi)]
         [(triangleright)  (send area add-polygon-glyph v r 3 0)]
         ; dots
         [(point pixel dot)     (send area set-pen color (* 1/2 r) 'solid)
                                (send area add-point v)]
         [(opoint opixel odot)  (send area set-pen color line-width 'solid)
                                (send area add-circle-glyph v r)
                                (send area set-pen
                                      color (* 9/4 line-width) 'solid)
                                (send area add-point v)]
         ; flares
         [(plus)        (send area add-flare-glyph v r 4 0)]
         [(times)       (send area add-flare-glyph v r 4 (* 1/4 pi))]
         [(5asterisk)   (send area add-flare-glyph v r 5 (* -1/2 pi))]
         [(asterisk)    (send area add-flare-glyph v r 6 (* -1/2 pi))]
         [(oplus)       (send area add-circle-glyph v (+ line-width r))
                        (send area add-flare-glyph v r 4 0)]
         [(otimes)      (send area add-circle-glyph v (+ line-width r))
                        (send area add-flare-glyph v r 4 (* 1/4 pi))]
         [(o5asterisk)  (send area add-circle-glyph v (+ line-width r))
                        (send area add-flare-glyph v r 5 (* -1/2 pi))]
         [(oasterisk)   (send area add-circle-glyph v (+ line-width r))
                        (send area add-flare-glyph v r 6 (* -1/2 pi))]
         ; arrows
         [(rightarrow)  (send area add-arrow-glyph v (+ 1 r) 0)]
         [(leftarrow)   (send area add-arrow-glyph v (+ 1 r) pi)]
         [(uparrow)     (send area add-arrow-glyph v (+ 1 r) (* -1/2 pi))]
         [(downarrow)   (send area add-arrow-glyph v (+ 1 r) (* 1/2 pi))]
         [else
          (raise-type-error 'points (format "symbol in ~a" known-point-symbols)
                            label)]))]
    [_  (raise-type-error 'add-points "string or symbol" label)]))
