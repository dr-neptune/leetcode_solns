#lang racket
(require racket)

(define letter-list
  (map (compose string integer->char)
       (inclusive-range (char->integer #\a) (char->integer #\z))))

(define cipher
  (append (map ~v (inclusive-range 1 9))
          (map (compose (curryr string-append "#") ~v) (inclusive-range 10 26))))

(define (freq-alphabets s)
  (foldr (λ (a b str) (string-replace str a b)) s cipher letter-list))
