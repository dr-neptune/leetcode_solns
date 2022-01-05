#lang racket
(require racket
         (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

;; fold solution
;; list -> integer -> add1 -> list
(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))


(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))


(define (plus-one digits)
  (int->digit-list (add1 (digit-list->int digits))))


(module+ test
  (require rackunit)
  (check-equal? (plus-one '(1 2 3)) '(1 2 4))
  (check-equal? (plus-one '(4 3 2 1)) '(4 3 2 2))
  (check-equal? (plus-one '(9)) '(1 0)))
