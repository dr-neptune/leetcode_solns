#lang racket
(require racket (rename-in racket [partition base/partition]))

(struct list-node
  (val next) #:mutable #:transparent)

(define (make-list-node [val 0])
  (list-node val #f))

 (require racket (rename-in racket [partition base/partition]))

(define (linked-list->numbers linked-list [values '()])
  (cond [(empty? linked-list) '()]
        [(list-node-next linked-list)
         (linked-list->numbers (list-node-next linked-list) (append values (list (list-node-val linked-list))))]
        [else (append values (list (list-node-val linked-list)))]))

(define (numbers->linked-list numbers)
  (define (build-up-list numbers built-up-list)
    (let ([value (make-list-node (first numbers))])
      (if (= 1 (length numbers))
          (begin
            (set-list-node-next! value built-up-list)
            value)
          (begin
            (set-list-node-next! value built-up-list)
            (build-up-list (rest numbers) value)))))
  (cond [(empty? numbers) '()]
        [(= 1 (length numbers)) (make-list-node (first numbers))]
        [else (build-up-list (rest (reverse numbers)) (make-list-node (last numbers)))]))

(define (partition head x)
  (if (or (not (list-node? head))
          (false? (list-node-next head)))
      head
      (let* ([ls (linked-list->numbers head)])
        ((compose numbers->linked-list
                  flatten
                  (curryr sort #:key car <)
                  (curry filter (compose not empty?))
                  (curryr call-with-values list))
         (λ () (base/partition (curry <= x) ls))))))


(partition (numbers->linked-list '(1 4 3 2 5 2)) 3)
(partition (numbers->linked-list '()) 0)
(partition (numbers->linked-list '(1)) 0)
(partition (numbers->linked-list '(1 1)) 0)
