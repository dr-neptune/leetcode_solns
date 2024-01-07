#lang racket
(require racket)


(define (max-sub-array nums [best -10000] [current 0])
    (if (empty? nums)
        best
        (let ([current-sum (max (first nums) (+ current (first nums)))])
          (max-sub-array (rest nums) (max best current-sum) current-sum))))

(let ([nums '(5 -3 5)])
  (let ([circ-nums (in-cycle nums)])
    (max-sub-array circ-nums)))
