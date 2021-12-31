#lang racket
(require racket)


(struct list-node
  (val next) #:mutable #:transparent)


(define (make-list-node [val 0])
  (list-node val #f))


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


(define (merge-two-lists list1 list2)
  (cond [(not (list-node? list1)) list2]
        [(not (list-node? list2)) list1]
        [else
         (let* ([list1 (linked-list->numbers list1)]
                [list2 (linked-list->numbers list2)]
                [combined-list (sort (append list1 list2) <)])
           (numbers->linked-list combined-list))]))


;; tests
(define list1 (list-node 1 (list-node 2 (make-list-node 4))))
(define list2 (list-node 1 (list-node 3 (make-list-node 4))))

(module+ test
  (require rackunit)
  ;; linked-list->numbers
  (check-equal? (linked-list->numbers '()) '())
  (check-equal? (linked-list->numbers (make-list-node 4)) '(4))
  (check-equal? (linked-list->numbers list1) '(1 2 4))
  ;; numbers->linked-list
  (check-equal? (numbers->linked-list '()) '())
  (check-equal? (numbers->linked-list '(1)) (make-list-node 1))
  (check-equal? (numbers->linked-list '(1 3 4)) list2)
  ;; merge-lists
  (check-equal? (merge-two-lists '() '()) '())
  (check-equal? (merge-two-lists '() (make-list-node)) (list-node 0 #f))
  (check-equal? (merge-two-lists list1 list2)
                (list-node 1 (list-node 1 (list-node 2 (list-node 3 (list-node 4 (list-node 4 #f))))))))
