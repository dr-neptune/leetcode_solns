#lang racket
(require racket)

(string->list "23")

(define *letters*
  (hash
   #\2 (string->list "abc")
   #\3 (string->list "def")
   #\4 (string->list "ghi")
   #\5 (string->list "jkl")
   #\6 (string->list "mno")
   #\7 (string->list "pqrs")
   #\8 (string->list "tuv")
   #\9 (string->list "wxyz")))

(define (letter-combinations digits)
  (if (equal? digits "") '()
      (map list->string
           (apply cartesian-product
                  (map (λ (d) (hash-ref *letters* d)) (string->list digits))))))

(letter-combinations "")
