#lang racket
(require racket
         (only-in srfi/1 map))

;; idea
;; pairwise map sgn of pairwise differences
(define exls '(1 2 2 3))
(define exls '(6 5 4 4))
(define exls '(1 3 2))

(require (only-in srfi/1 map))

(define (is-monotonic nums)
  (define (or-zero? sign)
    (λ (v) (or (sign v) (zero? v))))
  (define (check-sgn-diff sign)
    (λ (ls)
      (andmap (or-zero? sign)
              (map (compose sgn -) ls (cdr ls)))))
  (or ((check-sgn-diff positive?) nums)
      ((check-sgn-diff negative?) nums)))

(is-monotonic exls)
