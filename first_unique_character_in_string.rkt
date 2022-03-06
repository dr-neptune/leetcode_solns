#lang racket
(require racket)

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (lambda (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (first-uniq-char s)
  (let* ([vals (string->list s)]
         [counts (hash-table-counter vals)]
         [first-index (index-where vals (lambda (v) (= 1 (hash-ref counts v))))])
    (if first-index first-index -1)))

(first-uniq-char "leetcode")
(first-uniq-char "loveleetcode")
(first-uniq-char "aabb")
