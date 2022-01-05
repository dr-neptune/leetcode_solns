#lang racket
(require racket)

(define (add-binary a b)
  (~r (+ (string->number a 2)
         (string->number b 2)) #:base 2))

(module+ test
  (require rackunit)
  (check-equal? (add-binary "11" "1") "100")
  (check-equal? (add-binary "1010" "1011") "10101"))
