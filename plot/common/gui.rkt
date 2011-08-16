#lang racket/base

(require racket/gui)

(provide (all-defined-out))

(define (draw-text/anchor dc str x y
                          [anchor 'tl] [combine? #f] [offset 0] [angle 0])
  (define-values (width height baseline _2)
    (send dc get-text-extent str #f #t))
  (define dx
    (case anchor
      [(tl l bl top-left left bottom-left baseline-left)      0]
      [(t c b top center bottom baseline)                     (* 1/2 width)]
      [(tr r br top-right right bottom-right baseline-right)  width]))
  (define dy
    (case anchor
      [(tl t tr top-left top top-right)           0]
      [(l c r left center right)                  (* 1/2 height)]
      [(bl b br bottom-left bottom bottom-right)  height]
      [(baseline-left baseline-right baseline)    (- height baseline)]))
  (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
  (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
  (send dc draw-text str (- x rdx) (- y rdy) combine? offset angle))

(define read-only-text%
  (class text%
    (super-new)
    
    (send this hide-caret #t)
    
    (define writable? #t)
    
    (define/public (set-writable w?)
      (set! writable? w?))
    
    (define/augment (can-change-style? start len) writable?)
    (define/augment (can-delete? start len) writable?)
    (define/augment (can-insert? start len) writable?)
    (define/augment (can-load-file? filename format) writable?)
    (define/augment (can-save-file? filename format) writable?)
    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all)  #t]
        [else    writable?]))
    ))

(define (make-snip-frame snip width height label)
  (define frame (new frame% [width width] [height height] [label label]))
  (define text (new read-only-text%))
  (define canvas (new editor-canvas% [parent frame] [editor text]
                      [enabled #t]
                      [horizontal-inset 0] [horiz-margin 0]
                      [vertical-inset 0] [vert-margin 0]
                      [style '(no-hscroll no-vscroll no-border)]))
  (send text insert snip)
  (send text set-writable #f)
  frame)
