#lang racket
(require racket)

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

(define (reverse-between head left right)
  (define (values->list fn) (call-with-values fn list))
  (let ([num-ll (linked-list->numbers head)])
    (match-let ([(list f tail) (values->list (λ () (split-at num-ll (sub1 left))))])
      ((compose numbers->linked-list flatten (λ (lol) (list (first lol) (reverse (second lol)) (last lol))))
       (append (list f) (values->list (λ () (split-at-right tail (- (length num-ll) right)))))))))

(reverse-between (numbers->linked-list '(1 2 3 4 5)) 2 4)
(reverse-between (numbers->linked-list '(5)) 1 1)
