#lang racket/base

(require rackunit "zipper.rkt")

;; conses

(define cons-z (new-zipper (cons (cons 1 2) (cons 3 4))))

(check-equal? (zipper->value
               (zipper-set (unzip-car (unzip-cdr cons-z)) 30))
              (cons (cons 1 2) (cons 30 4)))

(check-equal? (zipper->value
               (zipper-set
                (unzip-cdr
                 (unzip-car
                  (zip (zipper-set (unzip-car (unzip-cdr cons-z)) 30) 2)))
                20))
              (cons (cons 1 20) (cons 30 4)))

;; proper lists

(define list-z (new-zipper (list (list 1 2) (list 3 4))))

(check-equal? (zipper->value
               (zipper-set (unzip-rest (unzip-first list-z))
                           (list 2 30 40)))
              (list (list 1 2 30 40) (list 3 4)))

(check-equal? (zipper->value
               (zipper-set (unzip-rest
                            (zip (zip (zipper-set (unzip-rest
                                                   (unzip-first list-z))
                                                  (list 2 30 40)))))
                           (list (list 300 400))))
              (list (list 1 2 30 40) (list 300 400)))

;; vectors

(check-equal? (zipper->value
               (zipper-set (unzip-vector
                            (unzip-vector
                             (new-zipper #(#(1 2 3) #(4 5 6))) 0) 1)
                           100))
              #(#(1 100 3) #(4 5 6)))

(check-equal? (zipper->value
               (zipper-set (unzip-vector-right (new-zipper #(0 1 2 3 4 5)) 3)
                           #(30 40 50)))
              #(0 1 2 30 40 50))

(check-equal? (zipper->value
               (zipper-set (unzip-vector-left (new-zipper #(0 1 2 3 4 5)) 3)
                           #(0 10 20)))
              #(0 10 20 3 4 5))

(check-equal? (zipper->value
               (zipper-set (unzip-subvector (new-zipper #(0 1 2 3 4 5)) 2 4)
                           #(a b c d e f)))
              #(0 1 a b c d e f 4 5))

(check-equal? (zipper->value
               (zipper-set
                (unzip-vector-left (new-zipper #(1 2 3 4 5)) 0)
                #(0)))
              #(0 1 2 3 4 5))

(check-equal? (zipper->value
               (zipper-set
                (unzip-vector-right (new-zipper #(1 2 3 4 5)) 5)
                #(6 7 8)))
              #(1 2 3 4 5 6 7 8))

;; strings

(check-equal? (zipper->value
               (zipper-set (unzip-string (new-zipper "012345") 3) #\T))
              "012T45")

(check-equal? (zipper->value
               (zipper-set (unzip-string-left
                            (new-zipper "Stronggrief Swordhaver") 11)
                           "Weakblade"))
              "Weakblade Swordhaver")

(check-equal? (zipper->value
               (zipper-set (unzip-string-right
                            (new-zipper "Stronggrief Swordhaver") 12)
                           "Tearscrier"))
              "Stronggrief Tearscrier")

(check-equal? (zipper->value
               (zipper-set (unzip-substring
                            (new-zipper "Brave Bigfrog Bullwetter") 6 13)
                           "Bratwurst"))
              "Brave Bratwurst Bullwetter")

(check-equal? (zipper->value
               (zipper-set
                (unzip-string-left (new-zipper "12345") 0)
                "0"))
              "012345")

(check-equal? (zipper->value
               (zipper-set
                (unzip-string-right (new-zipper "12345") 5)
                "678"))
              "12345678")

;; structs

(struct node (left right) #:transparent)

(define-struct-unzips node (left right))

(define node-z
  (new-zipper (node (node 1 2) (node 3 4))))

(check-equal? (zipper->value
               (zipper-set (unzip-node-right (unzip-node-left node-z)) 20))
              (node (node 1 20) (node 3 4)))
