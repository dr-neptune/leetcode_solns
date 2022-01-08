#lang racket
(require racket)

(define exls '(-10 -3 0 9 5))

(define (split-up nums)
  (let* ([middle-index (quotient (length nums) 2)]
         [left (take nums middle-index)]
         [right (list-tail nums (add1 middle-index))])
         (values left (list-ref nums middle-index) right)))

(define (sorted-array-to-bst nums)
  (if (empty? nums)
      #f
      (let-values ([(left middle right) (split-up nums)])
        (tree-node middle (sorted-array-to-bst left) (sorted-array-to-bst right)))))

(sorted-array-to-bst exls)
