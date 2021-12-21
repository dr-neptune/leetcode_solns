#lang racket

(require racket)

(define (val-indices ls vals)
  (remove-duplicates
   (flatten
    (for/list ([i vals])
      (indexes-of ls i)))))

(define (get-vals nums target)
  (let ([check (filter (lambda (n)
                         (= (first nums) (- target n)))
                       (rest nums))])
      (if (empty? check)
          (get-vals (rest nums) target)
          (list (first nums)
                (first check)))))


(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (val-indices nums (get-vals nums target)))


(define exls '(2 7 11 15))
(define exls2 '(3 2 4))
(define exls3 '(3 3))
(define exls4 '(2 5 5 11))

(two-sum exls 9)
(two-sum exls2 6)
(two-sum exls3 6)
(two-sum exls4 10)
