#lang racket/base

(require racket/draw racket/class)

(provide (all-defined-out))

(define (draw-text/anchor dc str x y
                          [anchor 'tl] [combine? #f] [offset 0] [angle 0])
  (define-values (width height _1 _2)
    (send dc get-text-extent str #f combine? offset))
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

(define (get-text-corners/anchor
         dc str x y [anchor 'tl] [combine? #f] [offset 0] [angle 0])
  (define-values (width height _1 _2)
    (send dc get-text-extent str #f combine? offset))
  (define dxs
    (case anchor
      [(tl l bl top-left left bottom-left)     (list 0 width)]
      [(t c b top center bottom)               (list (* -1/2 width)
                                                     (* 1/2 width))]
      [(tr r br top-right right bottom-right)  (list (- width) 0)]))
  (define dys
    (case anchor
      [(tl t tr top-left top top-right)           (list 0 height)]
      [(l c r left center right)                  (list (* -1/2 height)
                                                        (* 1/2 width))]
      [(bl b br bottom-left bottom bottom-right)  (list (- height) 0)]))
  (for*/list ([dx  (in-list dxs)] [dy  (in-list dys)])
    (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
    (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
    (vector (+ x rdx) (+ y rdy))))
