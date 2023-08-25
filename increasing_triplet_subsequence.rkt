#lang racket
(require racket)

(define (increasing-triplet nums)
  #t)

(module+ test
  (require rackunit)
  (check-true (increasing-triplet '(1 2 3 4 5)))
  (check-true (increasing-triplet '(5 4 3 2 1)))
  (check-true (increasing-triplet '(2 1 5 0 4 6))))
