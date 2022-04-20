#lang racket
(require racket)

(define (reverse-words s)
  (string-join (map (compose list->string reverse string->list) (string-split s))))
