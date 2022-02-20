#lang racket
(require racket
         (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

(define (in-i32-range i)
  (<= (expt -2 31) i (expt 2 31)))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (reverse-ls ls)
  (if (empty? ls)
      '()
      (append (reverse-ls (rest ls)) (list (first ls)))))

(define (reverse x)
  (let ([reversed-int (digit-list->int (reverse-ls (int->digit-list x)))])
    (if (in-i32-range reversed-int) reversed-int 0)))
