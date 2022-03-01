#lang racket
(require racket)

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (append (list (list-node-val ll))
              (linked-list->numbers (list-node-next ll)))))

(define (numbers->linked-list numbers)
  (if (empty? numbers) #f (foldr list-node #f numbers)))

(define (rotate-ls ls k)
  (cond [(> k (length ls)) (rotate-ls ls (remainder k (length ls)))]
        [else (append (take-right ls k) (take ls (- (length ls) k)))]))

(define (rotate-right head k)
  (numbers->linked-list (rotate-ls (linked-list->numbers head) k)))
