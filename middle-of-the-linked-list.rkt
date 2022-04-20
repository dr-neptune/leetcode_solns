#lang racket
(require racket)

(define exll (list-node 1 (list-node 2 (list-node 3 (list-node 4 (make-list-node 5))))))

(define (numbers->linked-list numbers)
  (if (empty? numbers) #f (foldr list-node #f numbers)))

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (cons (list-node-val ll)
            (linked-list->numbers (list-node-next ll)))))

(define (middle-node head)
  (let* ([ll (linked-list->numbers head)]
         [mid (quotient (length ll) 2)])
    (numbers->linked-list (drop ll mid))))

(middle-node exll)
