#lang racket
(require racket)

;; idea
;; if n == 1 then true
;; if n is int, then divide by 4
;; else false
(define (is-power-of-four n)
  (match n
    [0 #f]
    [1 #t]
    [(? integer?) (is-power-of-four (/ n 4))]
    [_ #f]))
