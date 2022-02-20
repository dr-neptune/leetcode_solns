#lang racket
(require racket)

;; o(1) space complexity and o(n) runtime complexity
;; start with the sum of the first n integers where n is the length of the input list
;; with each value, subtract the val from the sum
;; at the end, the result should be the missing value

(define (missing-number nums [total (/ (* (length nums) (add1 (length nums))) 2)])
  (if (empty? nums)
      total
      (missing-number (rest nums) (- total (first nums)))))


(define exls '(3 0 1))
(define exls2 '(0 1))
(define exls3 '(9 6 4 2 3 5 7 0 1))


(missing-number exls3)
