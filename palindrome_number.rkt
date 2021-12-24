#lang racket
(require racket)

(define (palindrome? n)
  (let* ([number->string-list (lambda (n) (string->list (number->string n)))]
        [fwd (number->string-list n)])
    (equal? fwd (reverse fwd))))
