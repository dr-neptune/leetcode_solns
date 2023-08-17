#lang racket
(require racket)

(define (judge-circle moves)
  (let loop ([x 0]
             [y 0]
             [motion (string->list moves)])
    (if (null? motion)
        (andmap zero? (list x y))
        (match (first motion)
          [#\U (loop x (add1 y) (rest motion))]
          [#\D (loop x (sub1 y) (rest motion))]
          [#\R (loop (add1 x) y (rest motion))]
          [#\L (loop (sub1 x) y (rest motion))]))))

(judge-circle "UD")
(judge-circle "LL")
