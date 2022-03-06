#lang racket
(require (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(define (subtract-product-and-sum n)
  (let ([digit-ls (int->digit-list n)])
    (- (apply * digit-ls)
       (apply + digit-ls))))

(subtract-product-and-sum 234)
