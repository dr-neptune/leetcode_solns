#lang racket
(require racket)

(define (is-power-of-two n)
  (let ([val (/ (log n)
                (log 2))])
    (< (abs
        (- val
           (exact-round val)))
       0.01)))


(is-power-of-two 536870912)


(is-power-of-two 255)

(define ex (/ (log 2147483647)
              (log 2)))

((abs (- ex (exact-round ex))))
