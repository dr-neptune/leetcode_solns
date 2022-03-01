#lang racket
(require racket)

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (append (list (list-node-val ll))
              (linked-list->numbers (list-node-next ll)))))

(define (numbers->linked-list numbers)
  (if (empty? numbers) #f (foldr list-node #f numbers)))

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (reverse-if-length k ls)
  (if (= k (length ls))
      (reverse ls)
      ls))

(define (reverse-k-group head k)
  (numbers->linked-list
   (flatten
    (map (curry reverse-if-length k)
         (split-into (linked-list->numbers head) k)))))

(define exll (numbers->linked-list '(1 2 3 4 5)))
(reverse-k-group exll 3)
