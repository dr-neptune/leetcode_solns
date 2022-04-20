#lang racket
(require racket)

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (append (list (list-node-val ll))
              (linked-list->numbers (list-node-next ll)))))

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (cons (list-node-val ll)
            (linked-list->numbers (list-node-next ll)))))

(define exll (list-node 1 (list-node 2 (list-node 2 (make-list-node 1)))))

(linked-list->numbers exll)
