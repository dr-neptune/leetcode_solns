#lang racket
(require racket)

(define (fizz-buzz-check n)
  (cond [(and (zero? (remainder n 5)) (zero? (remainder n 3))) "FizzBuzz"]
        [(zero? (remainder n 3)) "Fizz"]
        [(zero? (remainder n 5)) "Buzz"]
        [else (~v n)]))

(define (fizz-buzz n)
  (stream->list (stream-map fizz-buzz-check (in-range 1 (add1 n)))))

(define (fizz-buzz n)
  (map fizz-buzz-check (stream->list (in-range 1 (add1 n)))))

(fizz-buzz 15)
