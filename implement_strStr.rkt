#lang racket
(require racket)
(require (only-in srfi/13 string-contains))

(define (str-str haystack needle)
  (let ([search (string-contains haystack needle)])
    (if search search -1)))

(module+ test
  (require rackunit)
  (check-equal? (str-str "hello" "ll") 2)
  (check-equal? (str-str "aaaaa" "bba") -1)
  (check-equal? (str-str "" "") 0))
