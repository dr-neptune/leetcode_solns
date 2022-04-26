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

(define (remove-nth-from-end head n)
  (numbers->linked-list (flatten (list-update (linked-list->numbers head) n (λ (v) '())))))





(remove-nth-from-end exll 2)
(remove-nth-from-end (numbers->linked-list '(1)) 1)
(remove-nth-from-end (numbers->linked-list '(1 2)) 1)


(define (remove-nth-from-end head n)
  (let ([ll (linked-list->numbers head)])
    (numbers->linked-list (flatten (list-update ll (- (length ll) n) (λ (v) '()))))))


(remove-nth-from-end (numbers->linked-list '(1 2)) 1)
