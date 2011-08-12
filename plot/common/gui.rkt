#lang racket/base

(require racket/gui)

(provide (all-defined-out))

(define (draw-text/anchor dc str x y
                          [anchor 'tl] [combine? #f] [offset 0] [angle 0])
  (define-values (width height _1 _2)
    (send dc get-text-extent str #f #t))
  (define dx
    (case anchor
      [(tl l bl top-left left bottom-left)     0]
      [(t c b top center bottom)               (* 1/2 width)]
      [(tr r br top-right right bottom-right)  width]))
  (define dy
    (case anchor
      [(tl t tr top-left top top-right)           0]
      [(l c r left center right)                  (* 1/2 height)]
      [(bl b br bottom-left bottom bottom-right)  height]))
  (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
  (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
  (send dc draw-text str (- x rdx) (- y rdy) combine? offset angle))
