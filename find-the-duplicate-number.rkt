#lang racket
(require racket)

#|

dumb approach
for each number, check the rest of the list

|#

(let ([nums '(1 3 4 2 2)])
  (for/first ([num nums]
              [idx (in-inclusive-range 1 (length nums))]
              #:when (member num (drop nums idx)))
    (displayln (format "~a ~a" num (drop nums idx)))
    num))


(define (find-duplicate nums)
  (for/first ([num nums]
              [idx (in-inclusive-range 1 (length nums))]
              #:when (member num (drop nums idx)))
    num))

#|

great, dumb approach works. How can I do it in linear time?

maybe they specifically are using nums 1 -> n with n+1 digits?
if so, then we need to remove 1 instance of each number
and what is left is our duplicate.

still o(n^2)

the linear time approach shows the problem is isomorphic to finding where a cycle starts
in a linked list.

|#

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (find-duplicate nums)
  (for/first ([(k v) (in-hash (hash-table-counter nums))]
              #:when (> v 1))
    k))
