#lang racket
(require racket)

#|

idea

|#

(define exls '(1 2 3))
(define exls '(1 1 3 3 5 5 7 7))
(define exls '(1 1 2 2))

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (lambda (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (count-elements arr)
  (let ([hsh (hash-table-counter arr)])
    (for/sum ([ele (remove-duplicates arr)]
              #:do [(define prev-val-count (hash-ref hsh (sub1 ele) #f))]
              #:when prev-val-count)
      prev-val-count)))
