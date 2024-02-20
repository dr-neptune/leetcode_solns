#lang racket
(require racket)

(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(require (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

(define (numbers->linked-list numbers)
  (foldr list-node #f numbers))

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (append (list (list-node-val ll))
              (linked-list->numbers (list-node-next ll)))))

(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(define (add-two-numbers l1 l2)
  (define ll->int (compose digit-list->int linked-list->numbers))
  (let ([result (numbers->linked-list (int->digit-list (apply + (map ll->int (list l1 l2)))))])
    (if result result (make-list-node 0))))
