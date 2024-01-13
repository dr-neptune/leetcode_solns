#lang racket
(require racket)

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (lambda (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (val-filter ht predicate)
  (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

(define (largest-unique-number nums)
  (let* ([hsh (hash-table-counter nums)]
         [1s (val-filter hsh (λ (v) (equal? v 1)))])
    (if (zero? (hash-count 1s))
        -1
        (apply max (hash-keys 1s)))))
