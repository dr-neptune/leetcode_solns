#lang racket
(require racket)

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))


(define exls (list-node 1 (list-node 1 (make-list-node 2))))

;; quick approach first using old code
(define (linked-list->numbers linked-list [values '()])
  (cond [(empty? linked-list) '()]
        [(list-node-next linked-list)
         (linked-list->numbers (list-node-next linked-list)
                               (append values (list (list-node-val linked-list))))]
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

;; nodes -> numbers -> remove duplicates -> nodes
(define (delete-duplicates head)
  (if (not (list-node? head))
      head
      (numbers->linked-list (remove-duplicates (linked-list->numbers head)))))

;; new approach

;; iterate through list nodes
;; if the next value == the current value
;; then check the
