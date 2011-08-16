#lang racket/base

(require racket/contract racket/gui)

(provide (all-defined-out))

(define nonnegative-real/c (opt/c (and/c real? (>=/c 0))))
(define positive-real/c (opt/c (and/c real? (>/c 0))))
(define positive-integer/c (opt/c (and/c integer? (>/c 0))))

(define plot-color/c
  (opt/c (or/c (list/c nonnegative-real/c nonnegative-real/c nonnegative-real/c)
               symbol? string? (is-a?/c color%))))

(define brush-style/c
  (one-of/c 'transparent 'solid 'opaque 'xor 'hilite 'panel 'bdiagonal-hatch
            'crossdiag-hatch 'fdiagonal-hatch 'cross-hatch 'horizontal-hatch
            'vertical-hatch))

(define pen-style/c
  (one-of/c 'transparent 'solid 'xor 'hilite 'dot 'long-dash 'short-dash
            'dot-dash 'xor-dot 'xor-long-dash 'xor-short-dash 'xor-dot-dash))

(define font-family/c
  (one-of/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))
