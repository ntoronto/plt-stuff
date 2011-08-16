#lang racket/base

(require racket/gui racket/match
         "area.rkt")

(provide 3d-plot-snip%)    

(struct render-thread (state channel thread) #:mutable #:transparent)

(define (make-render-thread make-bm)
  (define ch (make-channel))
  (define th
    (thread
     (λ ()
       (let loop ()
         (match (channel-get ch)
           [(list angle altitude animating?)
            (channel-put ch (make-bm angle altitude animating?))]
           ['copy  (channel-put ch (make-render-thread make-bm))])
         (loop)))))
  (render-thread 'wait ch th))

(define (render-thread-get-bitmap r)
  (match-define (render-thread state ch th) r)
  (define bm (channel-get ch))
  (set-render-thread-state! r 'wait)
  bm)

(define (render-thread-try-get-bitmap r)
  (match-define (render-thread state ch th) r)
  (define bm (channel-try-get ch))
  (when bm (set-render-thread-state! r 'wait))
  bm)

(define (render-thread-wait r)
  (match-define (render-thread state ch th) r)
  (when (symbol=? state 'drawing)
    (render-thread-get-bitmap r)))

(define (render-thread-draw r angle altitude animating?)
  (render-thread-wait r)
  (match-define (render-thread state ch th) r)
  (channel-put ch (list angle altitude animating?))
  (set-render-thread-state! r 'drawing))

(define (render-thread-copy r)
  (render-thread-wait r)
  (match-define (render-thread state ch th) r)
  (channel-put ch 'copy)
  (channel-get ch))

(define 3d-plot-snip%
  (class image-snip%
    (init-field make-bm angle altitude
                [bm (make-bm angle altitude #f)]
                [rth (make-render-thread make-bm)])
    (inherit set-bitmap)
    
    (super-make-object bm)
    
    (define width (send bm get-width))
    (define height (send bm get-height))
    
    (define click-x 0)
    (define click-y 0)
    (define drag-x 0)
    (define drag-y 0)
    
    (define (new-angle)
      (let* ([angle  (+ angle (* (- drag-x click-x) (/ 180 width)))]
             [angle  (- angle (* (floor (/ angle 360)) 360))])
        (/ (round (* angle 10)) 10)))
    
    (define (new-altitude)
      (let* ([alt  (+ altitude (* (- drag-y click-y) (/ 180 height)))]
             [alt  (if (alt . < . 0) 0 (if (alt . > . 90) 90 alt))])
        (/ (round (* alt 10)) 10)))
    
    (define draw? #t)
    (define timer #f)
    
    (define ((update animating?))
      (define can-draw?
        (case (render-thread-state rth)
          [(wait)  #t]
          [(drawing)  (define new-bm (render-thread-try-get-bitmap rth))
                      (cond [new-bm  (set! bm new-bm)
                                     (set-bitmap bm)
                                     #t]
                            [else    #f])]))
      (when (and draw? can-draw?)
        (set! draw? #f)
        (render-thread-draw rth (new-angle) (new-altitude) animating?)))
    
    (define (stop-timer)
      (when timer
        (send timer stop)
        (set! timer #f)))
    
    (define (start-timer)
      (stop-timer)
      (set! timer (new timer%
                       [notify-callback (update #t)]
                       [interval 20])))
    
    (define/override (on-event dc x y editorx editory evt)
      (case (send evt get-event-type)
        [(left-down)  (render-thread-wait rth)
                      (set! click-x (send evt get-x))
                      (set! click-y (send evt get-y))
                      (set! drag-x click-x)
                      (set! drag-y click-y)
                      (set! draw? #t)
                      (start-timer)]
        [(left-up)    (stop-timer)
                      (set! draw? #f)
                      (render-thread-wait rth)
                      (set! drag-x (send evt get-x))
                      (set! drag-y (send evt get-y))
                      (set! angle (new-angle))
                      (set! altitude (new-altitude))
                      (render-thread-draw rth angle altitude #f)
                      (set! bm (render-thread-get-bitmap rth))
                      (set-bitmap bm)]
        [(motion)     (when timer
                        (set! drag-x (send evt get-x))
                        (set! drag-y (send evt get-y))
                        (set! draw? #t))]))
    
    (define/override (copy)
      (make-object 3d-plot-snip%
        make-bm angle altitude bm (render-thread-copy rth)))
    
    (define cross-cursor (make-object cursor% 'cross))
    (define blank-cursor (make-object cursor% 'blank))
    
    (define/override (adjust-cursor dc x y editorx editory evt)
      (if (send evt get-left-down)
          blank-cursor
          cross-cursor))
    
    (send this set-flags
          (cons 'handles-all-mouse-events (send this get-flags)))))
