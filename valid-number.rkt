#lang racket

(define (is-number s)
  (if (or (string-contains? s "f")
          (string-contains? s "L"))
      #f
      ((compose not false?) (string->number s))))


(is-number "b")
(is-number "1e6")
(is-number "8f8")
(is-number "1.99353L32")
