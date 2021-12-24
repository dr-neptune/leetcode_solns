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

(module+ test
  (require rackunit)
  (check-equal? (two-sum '(2 7 11 15) 9) '(0 1))
  (check-equal? (two-sum '(3 2 4) 6) '(1 2))
  (check-equal? (two-sum '(3 3) 6) '(0 1))
  (check-equal? (two-sum '(2 5 5 11) 10) '(1 2)))
