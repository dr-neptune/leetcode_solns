#lang racket
(require racket (only-in math/number-theory binomial))

(define (total-inc-dec x)
  (define number-length (sub1 (string-length (number->string (expt 10 x)))))
  (* 2 (binomial (+ number-length 9) number-length)))

;; not quit
