#lang racket
(require racket)

(define (array-sign nums) (sgn (foldl * 1 nums)))

(define exls '(-1 -2 -3 -4 3 2 1))

(array-sign exls)
