#lang racket
(require racket)

(define (vowel? c)
  (match c
    [(or #\a #\e #\i #\o #\u
         #\A #\E #\I #\O #\U) #t]
    [_ #f]))

;; no racket availability
