#lang racket
(require racket)

(define exlls (list (numbers->linked-list '(1 4 5))
                    (numbers->linked-list '(1 3 4))
                    (numbers->linked-list '(2 6))))

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (append (list (list-node-val ll))
              (linked-list->numbers (list-node-next ll)))))

(define (numbers->linked-list numbers)
  (if (empty? numbers) #f (foldr list-node #f numbers)))

(define (merge-k-lists lists)
  (numbers->linked-list (sort (flatten (map linked-list->numbers lists)) <)))
