#lang typed/racket

(require "../types.rkt"
         "../set-ops.rkt"
         "../arrow-transformer.rkt"
         "../branch-trace.rkt"
         "mapping.rkt"
         "preimage-mapping.rkt"
         "bot-arrow.rkt"
         "map-arrow.rkt"
         "pre-arrow.rkt"
         )

(provide (all-defined-out))

;; ===================================================================================================
;; Partial bottom arrow

(define-transformed-arrow
  Bot-Arrow arr/bot >>>/bot pair/bot if/bot lazy/bot agrees/bot π/bot
  Bot*-Arrow eta/bot* arr/bot* >>>/bot* pair/bot* if/bot* if*/bot* lazy/bot*)

(: ap/bot* (All (X Y) ((Bot*-Arrow X Y) X -> (Maybe Y))))
(define (ap/bot* f x)
  (define B (set-image (f j0) (set-product some-traces (set x))))
  (define C ((inst set-filter-out (just Y) Bottom) bottom? B))
  (if (set-empty? C) bottom (set-take C)))

(: id/bot* (All (X) (Bot*-Arrow X X)))
(define (id/bot* x)
  (((inst arr/bot* X X) (λ (x) x)) x))

(: const/bot* (All (X Y) (Y -> (Bot*-Arrow X Y))))
(define (const/bot* y)
  ((inst arr/bot* X Y) (λ (x) y)))

(: fst/bot* (All (X Y) (Bot*-Arrow (Pair X Y) X)))
(define (fst/bot* xy)
  (((inst arr/bot* (Pair X Y) X) car) xy))

(: snd/bot* (All (X Y) (Bot*-Arrow (Pair X Y) Y)))
(define (snd/bot* xy)
  (((inst arr/bot* (Pair X Y) Y) cdr) xy))

;; ===================================================================================================
;; Partial mapping arrow

(define-transformed-arrow
  Map-Arrow arr/map >>>/map pair/map if/map lazy/map agrees/map π/map
  Map*-Arrow eta/map* arr/map* >>>/map* pair/map* if/map* if*/map* lazy/map*)

(: lift/map* (All (X Y) ((Bot*-Arrow X Y) -> (Map*-Arrow X Y))))
(define ((lift/map* f) j)
  (lift/map (f j)))

(: id/map* (All (X) (Map*-Arrow X X)))
(define (id/map* x)
  (((inst arr/map* X X) (λ (x) x)) x))

(: const/map* (All (X Y) (Y -> (Map*-Arrow X Y))))
(define (const/map* y)
  ((inst arr/map* X Y) (λ (x) y)))

(: fst/map* (All (X Y) (Map*-Arrow (Pair X Y) X)))
(define (fst/map* xy)
  (((inst arr/map* (Pair X Y) X) car) xy))

(: snd/map* (All (X Y) (Map*-Arrow (Pair X Y) Y)))
(define (snd/map* xy)
  (((inst arr/map* (Pair X Y) Y) cdr) xy))

;; ===================================================================================================
;; Partial preimage arrow

(define-transformed-arrow
  Pre-Arrow arr/pre >>>/pre pair/pre if/pre lazy/pre agrees/pre π/pre
  Pre*-Arrow eta/pre* arr/pre* >>>/pre* pair/pre* if/pre* if*/pre* lazy/pre*)

(: lift/pre* (All (X Y) ((Map*-Arrow X Y) -> (Pre*-Arrow X Y))))
(define ((lift/pre* f) j)
  (lift/pre (f j)))

(: id/pre* (All (X) (Pre*-Arrow X X)))
(define (id/pre* x)
  (((inst arr/pre* X X) (λ (x) x)) x))

(: const/pre* (All (X Y) (Y -> (Pre*-Arrow X Y))))
(define (const/pre* y)
  ((inst arr/pre* X Y) (λ (x) y)))

(: fst/pre* (All (X Y) (Pre*-Arrow (Pair X Y) X)))
(define (fst/pre* xy)
  (((inst arr/pre* (Pair X Y) X) car) xy))

(: snd/pre* (All (X Y) (Pre*-Arrow (Pair X Y) Y)))
(define (snd/pre* xy)
  (((inst arr/pre* (Pair X Y) Y) cdr) xy))
