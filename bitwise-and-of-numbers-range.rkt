#lang racket
(require racket)


(let ([lhs 5]
      [rhs 7])
  (apply bitwise-and (stream->list (in-inclusive-range lhs rhs))))

(define (range-bitwise-and left right)
  (foldl bitwise-and (inclusive-range left right)))


(define exlhs 1)
(define exrhs 2147483647)

(range-bitwise-and exlhs exrhs)

(bitwise-and exlhs exrhs)

(bitwise-and (number->string 10 2) (number->string 10 2))

#|

4: 100
7: 111

4 & 7 = 100 & 111 = 100

suppose we have 1 & 25
1:   0...1
25:  11001

|#
