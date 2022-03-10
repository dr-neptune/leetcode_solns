#lang racket
(require racket)

(string-downcase "HELLO")

(define (to-lower-case s)
  (string-downcase s))

;; without builtin function
(define (to-lower-case s)
  (let rc ([str (string->list s)] [new-str '()])
    (cond [(empty? str) (list->string new-str)]
          [(char-upper-case? (first str))
           (rc (rest str) (append new-str (list (char-downcase (first str)))))]
          [else (rc (rest str) (append new-str (list (first str))))])))
