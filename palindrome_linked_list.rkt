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


(define (is-palindrome head)
  (let ([numls (linked-list->numbers head)])
    (equal? numls (reverse numls))))


(define exll (list-node 1 (list-node 2 (list-node 2 (make-list-node 1)))))

(is-palindrome exll)
