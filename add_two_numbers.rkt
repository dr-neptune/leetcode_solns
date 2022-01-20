#lang racket
(require racket
         (only-in srfi/1 unfold-right)
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
  (foldr list-node #f numbers))

(define (linked-list->numbers ll)
  (if (not (list-node? ll))
      '()
      (append (list (list-node-val ll))
              (linked-list->numbers (list-node-next ll)))))

(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(define (add-two-numbers l1 l2)
  (define ll->int (compose digit-list->int reverse linked-list->numbers))
  (let ([result ((compose numbers->linked-list reverse int->digit-list +) (ll->int l1) (ll->int l2))])
    (if result result (make-list-node 0))))


(define exls1 (numbers->linked-list '(2 4 3)))
(define exls2 (numbers->linked-list '(5 6 4)))

(add-two-numbers (numbers->linked-list '(0))
                 (numbers->linked-list '(0)))

(add-two-numbers exls1 exls2)
