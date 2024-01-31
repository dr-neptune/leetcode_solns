#lang racket
(require racket)


(define (are-sentences-similar sentence1 sentence2 similarPairs)
  (let ([sorted-pairs (map (curryr sort string<?) similarPairs)])
    (if (not (equal? (length sentence1) (length sentence2)))
        #f
        (for/and ([s1 sentence1] [s2 sentence2])
          (if (equal? s1 s2)
              #t
              (not (false? (member (sort (list s1 s2) string<?) sorted-pairs))))))))
