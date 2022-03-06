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

(define (numbers->linked-list numbers)
  (if (empty? numbers) #f (foldr list-node #f numbers)))

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (val-filter ht predicate)
  (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

(define (delete-duplicates head)
  (let ([counts (hash-table-counter (linked-list->numbers head))])
    ((compose1
      numbers->linked-list
      (curryr sort <)
      hash-keys
      (curryr val-filter (λ (v) (= v 1))))
     counts)))

(define exls '(1 2 3 3 4 4 5))
(define exll (numbers->linked-list exls))

(delete-duplicates exll)
