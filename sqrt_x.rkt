#lang racket
(require racket)

;; base library
(define (my-sqrt x)
  (integer-sqrt x))

;; newton raphson integer method
(define (my-sqrt x)
  (define (integer-newton r x)
    (if (> (* r r) x)
        (integer-newton (quotient (+ r (quotient x r)) 2) x)
        r))
  (integer-newton x x))
