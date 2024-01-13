#lang racket
(require racket)

(require (only-in srfi/1 unfold-right))

(define (int->digit-list int)
  (unfold-right zero? (curryr remainder 10) (curryr quotient 10) int))

(define (digit-list->int ls)
  (foldl (λ (digit power) (+ (* 10 power) digit)) 0 ls))

(define rotate-hsh
  (make-hash '((0 0) (1 1) (6 9) (8 8) (9 6))))

(define (is-strobogrammatic num)
  (let* ([num (string->number num)]
        [dig-ls (int->digit-list num)])
    (if (not (andmap (λ (v) (member v (hash-keys rotate-hsh))) dig-ls))
        #f
        ((compose (curry equal? num) digit-list->int reverse flatten)
         (map (curry hash-ref rotate-hsh) dig-ls)))))
