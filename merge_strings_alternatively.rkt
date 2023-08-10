#lang racket
(require racket)

(define (zip w1 w2)
  (cond [(= (length w1) (length w2)) (map list w1 w2)]
        [(> (length w1) (length w2))
         (append (map list (take w1 (length w2)) w2)
                 (drop w1 (length w2)))]
        [else (append (map list w1 (take w2 (length w1)))
                      (drop w2 (length w1)))]))

(define (merge-alternately word1 word2)
  (let ([w1 (string->list word1)]
        [w2 (string->list word2)])
    (list->string (flatten (zip w1 w2)))))



(merge-alternately "abc" "pqr")
(merge-alternately "ab" "pqrs")
(merge-alternately "abcd" "pq")

;; try again
(define (merge-alternately word1 word2)
  (list->string
   (let loop ([w1 (string->list word1)]
              [w2 (string->list word2)])
     (match (list w1 w2)
       [(list '() '()) '()]
       [(list a '()) a]
       [(list '() b) b]
       [_ (cons (first w1) (cons (first w2) (loop (rest w1) (rest w2))))]))))

(merge-alternately "abc" "pqr")
(merge-alternately "ab" "pqrs")
(merge-alternately "abcd" "pq")

;; woohoo, 100% in both time and memory
