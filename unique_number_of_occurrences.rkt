#lang racket
(require racket)

(define exls '(1 2 2 1 1 3))

;; idea
;; counter -> values == set(values)
(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (unique-occurrences arr)
  (let* ([counter (hash-table-counter arr)]
         [value-counts (hash-values counter)])
    (eq? (length value-counts) (length (remove-duplicates value-counts)))))

(unique-occurrences '(1 2 2 1 1 3))
