#lang racket
(require racket)

#|

idea

pad all words to be full length

for i, j, i == j, iterate through. See if horizontal == vertical

|#

(define exsquare '("abcd" "bnrt" "crm" "dt"))
(define exsquare '("ball" "area" "read" "lady"))

(define (valid-word-square words)
  (let* ([max-word-len (apply max (map string-length words))]
         [f-words (map (compose
                        string->list
                        (λ (word) (if (< (string-length word) max-word-len)
                                      (string-append word (make-string (- max-word-len (string-length word)) #\0))
                                      word)))
                       words)]
         [equal-cross-section?
          (λ (x y)
            (let ([horizontal (list-ref f-words x)]
                  [vertical (map (curryr list-ref y) f-words)])
              (equal? horizontal vertical)))])
    (for/and ([i (in-range (length f-words))])
      (equal-cross-section? i i))))
