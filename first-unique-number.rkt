#lang racket
(require racket)

#|

idea
cons new values onto queue
keep a counter handy
so that way when we check if the first num is unique, we can tell without traversing the list

|#

(require data/queue)

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (lambda (v) (hash-update! counts v add1 0)) ls)
    counts))

(define first-unique%
  (class object%
    (super-new)

    ; nums : (listof exact-integer?)
    (init-field
     nums
     [nums-q (make-queue)]
     [counter (hash-table-counter nums)])

    (for ([num nums])
      (enqueue! nums-q num))

    ; show-first-unique : -> exact-integer?
    (define/public (showFirstUnique)
      (if (queue-empty? nums-q)
          -1
          (let ([q-first (dequeue! nums-q)])
            (begin
              (enqueue-front! nums-q q-first)
              q-first))))
    ; add : exact-integer? -> void?
    (define/public (add value)
      (queue-filter! nums-q (λ (v) (if (equal? 1 (hash-ref counter v))
                                       #f
                                       (begin
                                         (hash-set! counter v 0)
                                         #t)))))))


(define obj (new first-unique% [nums '(2 3 5)]))
(define param_1 (send obj showFirstUnique))
(send obj add 5)
(send obj add 2)
(send obj add 3)
;; (send obj add 7)
;; (send obj add 17)
(define param_1 (send obj showFirstUnique))
