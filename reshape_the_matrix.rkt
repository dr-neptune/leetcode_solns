#lang racket
(require racket)

(define (split-into ls subls-size)
  (if (= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (matrix-reshape mat r c)
  (let ([flatmat (flatten mat)])
    (if (not (= (* r c) (length flatmat)))
        mat
        (filter (compose not empty?) (split-into flatmat (quotient (length flatmat) r))))))

(matrix-reshape '((1 2)(3 4)) 1 4)
(matrix-reshape '((1 2)(3 4)) 2 4)
(matrix-reshape '((1 2 3)(4 5 6)) 3 2)
(matrix-reshape '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16) (17 18 19 20)) 42 5)
