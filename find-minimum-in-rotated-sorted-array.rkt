#lang racket
(require racket)

(define exls '(4 5 6 7 0 1 2))

(define (find-min nums)
  (let rc ([left 0] [right (sub1 (length nums))])
    (let ([mid (quotient (+ left right) 2)])
      (cond [(>= left right) (list-ref nums left)]
            [(< (list-ref nums mid) (list-ref nums right))
             (rc left mid)]
            [else (rc (add1 mid) right)]))))

(find-min exls)
(find-min '(5 1 2 3 4))
(find-min '(11 13 15 17))



'(3 4 5 1 2)
;; trace
;; l 0 r 4 m 2 mv 5
;; 5 > 1
;; l 3 r 4 m 3 mv 1
;; 1 < 2
;; l 3 r 3 m 3 mv 1
;; return 1
