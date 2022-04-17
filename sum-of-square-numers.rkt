#lang racket
(require racket)

;; using sqrt
(define (square x) (* x x))

(define (judge-square-sum c)
  (let rc ([a 0])
    (let ([b (sqrt (- c (square a)))])
      (cond [(> (square a) c) #f]
            [(exact-integer? b) #t]
            [else (rc (add1 a))]))))

(judge-square-sum 0)
