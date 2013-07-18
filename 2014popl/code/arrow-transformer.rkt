#lang typed/racket

(require "types.rkt"
         "set-ops.rkt"
         "branch-trace.rkt")

(provide (all-defined-out))

(define-syntax-rule (define-transformed-arrow
                      In-Arrow arr/a >>>/a pair/a if/a lazy/a agrees/a π/a
                      Out-Arrow eta/a* arr/a* >>>/a* pair/a* if/a* if*/a* lazy/a*)
  (begin
    (define-type (Out-Arrow X Y) (Tree-Index -> (In-Arrow (Pair Branch-Trace X) Y)))
    
    (: fst/a (All (X Y) (In-Arrow (Pair X Y) X)))
    (define (fst/a xy)
      (((inst arr/a (Pair X Y) X) car) xy))
    
    (: snd/a (All (X Y) (In-Arrow (Pair X Y) Y)))
    (define (snd/a xy)
      (((inst arr/a (Pair X Y) Y) cdr) xy))
    
    (: eta/a* (All (X Y) ((In-Arrow X Y) -> (Out-Arrow X Y))))
    (define ((eta/a* f) j)
      (>>>/a (inst snd/a Branch-Trace X) f))
    
    (: arr/a* (All (X Y) ((X -> Y) -> (Out-Arrow X Y))))
    (define (arr/a* f)
      (eta/a* ((inst arr/a X Y) f)))
    
    (: >>>/a* (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow Y Z) -> (Out-Arrow X Z))))
    (define ((>>>/a* f1 f2) j)
      (>>>/a (pair/a (inst fst/a Branch-Trace X) (f1 (left j))) (f2 (right j))))
    
    (: pair/a* (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow X Z) -> (Out-Arrow X (Pair Y Z)))))
    (define ((pair/a* f1 f2) j)
      (pair/a (f1 (left j)) (f2 (right j))))
    
    (: lazy/a* (All (X Y) ((-> (Out-Arrow X Y)) -> (Out-Arrow X Y))))
    (define ((lazy/a* f) j) (lazy/a (λ () ((f) j))))
    
    (: if/a* (All (X Y) ((Out-Arrow X Boolean) (Out-Arrow X Y) (Out-Arrow X Y) -> (Out-Arrow X Y))))
    (define ((if/a* f1 f2 f3) j)
      (if/a (f1 (left j)) (f2 (left (right j))) (f3 (right (right j)))))
    
    (: branch/a* (All (X) (Out-Arrow X Boolean)))
    (define (branch/a* j)
      (>>>/a (inst fst/a Branch-Trace X) (π/a j)))
    
    (: agrees/a* (All (X) (Out-Arrow (Pair Boolean Boolean) Boolean)))
    (define (agrees/a* j)
      (((inst eta/a* (Pair Boolean Boolean) Boolean) agrees/a) j))
    
    (: if*/a* (All (X Y) ((Out-Arrow X Boolean) (Out-Arrow X Y) (Out-Arrow X Y) -> (Out-Arrow X Y))))
    (define (if*/a* f1 f2 f3)
      (if/a* (>>>/a* (pair/a* f1 (inst branch/a* X)) agrees/a*) f2 f3))
    
    ))
