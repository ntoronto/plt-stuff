#lang racket/base

(require racket/class)

(provide (all-defined-out))

(define full-glyph-hash
  #hash((fullcircle . circle)
        (fullsquare . square)
        (fulldiamond . diamond)
        (fulltriangle . triangle)
        (fulltriangleup . triangleup)
        (fulltriangledown . triangledown)
        (fulltriangleleft . triangleleft)
        (fulltriangleright . triangleright)
        (full4star . 4star)
        (full5star . 5star)
        (full6star . 6star)
        (full7star . 7star)
        (full8star . 8star)))

(define known-point-symbols
  '(dot point pixel
        plus times asterisk 5asterisk
        oplus otimes oasterisk o5asterisk
        circle square diamond triangle
        fullcircle fullsquare fulldiamond fulltriangle
        triangleup triangledown triangleleft triangleright
        fulltriangleup fulltriangledown fulltriangleleft fulltriangleright
        rightarrow leftarrow uparrow downarrow
        4star 5star 6star 7star 8star
        full4star full5star full6star full7star full8star))
