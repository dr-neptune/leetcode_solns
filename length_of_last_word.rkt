#lang racket
(require racket)
(require threading)


(define exstr "Hello World")


;; without threading
(define (length-of-last-word s) (string-length (last (string-split s))))


;; with threading
(define (length-of-last-word s)
  (~> s
      string-split
      last
      string-length))
