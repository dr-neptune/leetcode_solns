#lang racket
(require racket)

(define (maximum-wealth accounts)
  (apply max (map (curry apply +) accounts)))
