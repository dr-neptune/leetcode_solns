#lang racket
(require racket)

(define (zip w1 w2)
  (cond [(= (length w1) (length w2)) (map list w1 w2)]
        [(> (length w1) (length w2))
         (append (map list (take w1 (length w2)) w2)
                 (drop w1 (length w2)))]
        [else (append (map list w1 (take w2 (length w1)))
                      (drop w2 (length w1)))]))

(define (merge-alternatively word1 word2)
  (let ([w1 (string->list word1)]
        [w2 (string->list word2)])
    (list->string (flatten (zip w1 w2)))))

(merge-alternatively "abc" "pqr")
(merge-alternatively "ab" "pqrs")
(merge-alternatively "abcd" "pq")
