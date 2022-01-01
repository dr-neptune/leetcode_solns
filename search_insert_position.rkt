#lang racket
(require racket)

;; binary search solution in O(logn)
(define (search-insert nums target [low 0] [high (sub1 (length nums))])
  (if (< high low)
      low
      (let ([mid (floor (/ (+ low high) 2))])
        (if (>= (list-ref nums mid) target)
            (search-insert nums target low (sub1 mid))
            (search-insert nums target (add1 mid) high)))))


;; other racketeer solution in O(n)
(define (search-insert nums target [acc 0])
  (match nums
    [(cons x xs) #:when (< x target) (search-insert xs target (add1 acc))]
    [else acc]))

;; tests
(define exls '(1 3 5 6))

(module+ test
  (require rackunit)
  (check-equal? (search-insert exls 1) 0)
  (check-equal? (search-insert exls 3) 1)
  (check-equal? (search-insert exls 5) 2)
  (check-equal? (search-insert exls 6) 3))
