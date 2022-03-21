#lang racket
(require racket)

;; official solution
;; repeated subtraction

;; helper functions
(define count-negatives
  (λ vals (foldl + 0 (map (compose (λ (b) (match b [#t 1] [#f 0])) negative?) vals))))

(define (make-negative int) (if (> int 0) (- int) int))

;; core algorithm
(define (sub-div curr-dividend divisor [divisions 0])
  (if (<= (- curr-dividend divisor) 0)
      (sub-div (- curr-dividend divisor) divisor (sub1 divisions))
      divisions))

(define (divide dividend divisor)
  (if (and (= dividend -2147483648) (= divisor -1)) 2147483647
      (let* ([negatives (- 2 (count-negatives dividend divisor))]
             [dividend (make-negative dividend)]
             [divisor (make-negative divisor)]
             [negative-division (sub-div dividend divisor)])
        (if (= negatives 1) negative-division (- negative-division)))))

;; repeated exponential searches
(define (sub-div curr-dividend divisor [divisions 0])
  (if (>= divisor curr-dividend)
      (let lp ([power-of-two -1]
               [value divisor])
        (if (and (>= value -1073741824)
                 (>= (+ value value) curr-dividend))
            (lp (+ power-of-two power-of-two)
                (+ value value))
            (sub-div (- curr-dividend value) divisor (+ divisions power-of-two))))
      divisions))

;; adding powers of two
;; tbd?

(divide -2147483648 -1)
(divide 2147483648 1)
(divide -2147483648 1)
(divide 2147483648 -1)
(divide 10 3)
(divide 10 -3)
(divide -10 -3)
