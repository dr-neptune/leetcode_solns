#lang racket
(require racket)

;; boo! all these in-place questions are lame
(define (reverse-string s)
  (list->string (reverse (string->list s))))

(reverse-string "hello")
