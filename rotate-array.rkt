#lang racket
(require racket)

;; idea
;; take-right k values
;; cons to the front

(define exls '(1 2 3 4 5 6 7))

(append (take-right exls 3) (drop-right exls 3))

(define (rotate nums k)
  (append (take-right nums k) (drop-right nums k)))
