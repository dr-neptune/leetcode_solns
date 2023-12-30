#lang racket
(require racket)

(define (min-sub-array-len target nums)
  (let ([nums (list->vector nums)]
        [max-val (expt 10 9)])
    (let loop ([idx 0] [sum 0] [left 0] [answer max-val])
      (cond [(>= sum target)
             (loop idx (- sum (vector-ref nums left)) (add1 left) (min answer (- idx left)))]
            [(> idx (sub1 (vector-length nums))) (if (= answer max-val) 0 answer)]
            [else (loop (add1 idx) (+ sum (vector-ref nums idx)) left answer)]))))
