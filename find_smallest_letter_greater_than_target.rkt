#lang racket
(require racket)

(define (next-greatest-letter letters target)
  (if (char<=? (last letters) target)
      (first letters)
      (findf (λ (l) (char<? target l)) letters)))
