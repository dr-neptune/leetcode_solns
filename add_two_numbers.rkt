#lang racket

(require (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

;; TODO
;; get it working with the stripped down numbers->linked-list function

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define (numbers->linked-list numbers)
  (foldl list-node #f numbers))

(define (linked-list->numbers linked-list [values '()])
  (cond [(empty? linked-list) '()]
        [(list-node-next linked-list)
         (linked-list->numbers (list-node-next linked-list) (append values (list (list-node-val linked-list))))]
        [else (append values (list (list-node-val linked-list)))]))

(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(define (add-two-numbers l1 l2)
  (let* ([flist (linked-list->numbers l1)]
         [llist (linked-list->numbers l2)]
         [result (numbers->linked-list
                  (reverse
                   (int->digit-list
                    (+ (digit-list->int (reverse flist))
                       (digit-list->int (reverse llist))))))])
    (if (empty? result)
        (make-list-node 0)
        result)))

(define (add-two-numbers l1 l2)
  (let* ([flist (linked-list->numbers l1)]
         [llist (linked-list->numbers l2)]
         [result (numbers->linked-list
                  (reverse
                   (int->digit-list
                    (+ (digit-list->int (reverse flist))
                       (digit-list->int (reverse llist))))))])
    (if (empty? result)
        (make-list-node 0)
        result)))

(define (add-two-numbers l1 l2)
  (let* ([flist (linked-list->numbers l1)]
         [llist (linked-list->numbers l2)]
         [result ((compose numbers->linked-list int->digit-list)
                  (+ (digit-list->int (reverse flist))
                     (digit-list->int (reverse llist))))])
    (if (empty? result)
        (make-list-node 0)
        result)))


(add-two-numbers exls1 exls2)


(numbers->linked-list (make-list-node))

(define exls1 (numbers->linked-list '(2 4 3)))
(define exls2 (numbers->linked-list '(5 6 4)))

(numbers->linked-list (reverse (int->digit-list (add-two-numbers exls1 exls2))))

(add-two-numbers (numbers->linked-list '(0))
                 (numbers->linked-list '(0)))

(add-two-numbers (make-list-node) (make-list-node))

(list-node-val exls1)
