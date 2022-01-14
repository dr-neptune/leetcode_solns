#lang racket
(require racket)

(define exprices '(7 1 5 3 6 4))
(define exprices2 '(7 6 4 3 1))
(define exprices3 '(1 4 1 4 3 1))

(define (max-profit prices [max-cur 0] [max-so-far 0])
  (if (empty? (rest prices))
      max-so-far
      (let* ([cur (max 0 (+ max-cur (- (second prices) (first prices))))]
             [so-far (max cur max-so-far)])
        (max-profit (rest prices) cur so-far))))

(max-profit exprices)
(max-profit exprices2)
(max-profit exprices3)
