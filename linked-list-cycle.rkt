#lang racket
(require racket)

(define (numbers->linked-list numbers)
  (if (empty? numbers) #f (foldr list-node #f numbers)))

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (cons (list-node-val ll)
            (linked-list->numbers (list-node-next ll)))))

;; idea
;; make a fast pointer and a slow pointer
;; the fast pointer jumps ahead 2 steps, whereas the slow point jumps ahead 1 step
;; if there is a final node, then the fast pointer will find it
;; if the fast pointer meets the slow pointer, then we have a cycle.
;; this does not work if there are duplicate elements in the list

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define ll (list-node 3 (list-node 2 (list-node 0 (list-node -4 (list-node-next ll))))))
