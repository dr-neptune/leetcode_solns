#lang racket
(require racket)

(define (triangle-check ls)
  (let ([a (first ls)] [b (second ls)] [c (last ls)])
    (and (> (+ a b) c)
         (> (+ a c) b)
         (> (+ b c) a))))

(define (largest-perimeter nums)
  (let rc ([ls (reverse (sort nums <))])
    (if (< (length ls) 3) 0
        (let ([first-3 (take ls 3)])
          (if (triangle-check first-3)
              (apply + first-3)
              (rc (rest ls)))))))
